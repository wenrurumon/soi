
rm(list=ls())
library(reticulate)
library(dplyr)
library(data.table)
library(pyramidi)
library(stringr)
library(tuneR)
library(seewave)

use_python('/Users/huzixin/anaconda3/envs/test/bin/python')
setwd('/Users/huzixin/Documents/soi/data/beyond')
pydocx <- import('docx')
m21 <- import('music21')

################################################################################
# Modules
################################################################################

readsong <- function(songname){
  x <- pydocx$Document(songname)  
  x <- sapply(x$paragraphs,function(x){x$text})
  x <- x[sapply(x,nchar)>0]
  list(
    info = x[1:(grep('简谱',x)-1)],
    flow = x[(grep('简谱',x)+1):(grep('和旋谱|和弦谱',x)-1)],
    chord = x[(grep('和旋谱|和弦谱',x)+1):length(x)]
  )
}

parseflow <- function(x){
  x <- unlist(strsplit(x,'\\|'))[-1] %>% strsplit('')
  lapply(x,function(xi){
    xi <- xi[xi!=' ']
    if(length(xi)==0){
      return(NULL)
    } else {
      flowi <- data.table(flow=xi,t(sapply(1:length(xi),function(i){
        c(sum(xi[1:i]=='('),sum(xi[1:i]==')'),
          sum(xi[1:i]=='['),sum(xi[1:i]==']'))
      }))) %>%
        mutate(speed=V1-V2) %>%
        mutate(speed=1/(2^speed)) %>%
        mutate(speed=ifelse(flow=='.',speed/2,speed)) %>%
        mutate(speed2=V3-V4) %>%
        mutate(speed=ifelse(speed2==1,speed/3,speed)) %>%
        select(flow,speed)
      flowi$flow[which(flowi$flow%in%c('+','-'))-1] <- paste0(flowi$flow[which(flowi$flow%in%c('+','-'))-1],flowi$flow[which(flowi$flow%in%c('+','-'))])
      flowi$flow[which(flowi$flow%in%c('#','b'))+1] <- paste0(flowi$flow[which(flowi$flow%in%c('#','b'))],flowi$flow[which(flowi$flow%in%c('#','b'))+1])
      flowi <- flowi %>% filter(!flow%in%c('(',')','+','-','#','b','[',']','~+'))
      flowi$time <- cumsum(flowi$speed)-flowi$speed[1]+1
      flowi 
    }
  })
}

tonote <- function(x){
  basenotes <- strsplit('1,#1,2,#2,3,4,#4,5,#5,6,#6,7',',')[[1]]  
  xbase <- match(gsub('\\+|\\-','',x),basenotes)
  xbase <- 261.63 * 2^((xbase - 1) / 12)
  xbase <- xbase * (str_count(x, "\\+")+1)
  xbase <- xbase / (str_count(x, "\\-")+1)
  return(xbase)
}

processwav <- function(X){
  # X <- flowi
  X <- do.call(rbind,lapply(1:nrow(X),function(i){
    rbind(
      X[i,] %>% mutate(speed=speed-speed*0.1),
      X[i,] %>% mutate(flow=0,time=time+speed-speed*0.1,speed=speed*0.1)
    )
  }))
  x <- X %>%
    mutate(flow=ifelse(flow%in%c('~','.','--','、'),0,flow))
  x$key <- ifelse(x$flow==0,0,tonote(x$flow))
  x <- x[min(which(x$flow!='0')):nrow(x),]
  sound_list <- lapply(1:nrow(x), function(i) {
    # print(i)
    if(x$key[i]==0){
      sine_wave <- sine(99999, duration = x$speed[i]/2*44100, samp.rate = 44100)
      sine_wave@left[] <- 0
      sine_wave
      # Wave(left = rep(0,x$speed[i]/2*44100), samp.rate = 44100,bit=32,pcm=T)
    } else {
      sine_wave <- sine(x$key[i], duration = x$speed[i]/2*44100, samp.rate = 44100)
      sine_wave
      # Wave(left = as.numeric(sine_wave@left), samp.rate = 44100, bit = 32, pcm = TRUE)
    }
  })
  sound_list <- sound_list[!sapply(sound_list,is.null)]
  out <- do.call('bind',sound_list)
  list(flow=x,wave=out)
}

################################################################################
# Process
################################################################################

files <- dir(pattern='docx')
songs <- lapply(files,readsong)
names(songs) <- files
songs <- songs[which(sapply(songs,function(x){sapply(x,length)})[2,]==sapply(songs,function(x){sapply(x,length)})[3,])]
flows <- lapply(songs,function(x){x$flow})
flows <- lapply(flows,parseflow)

#Get Music

names(flows)
flowi <- do.call(rbind,flows[[17]])
wavei <- processwav(flowi)
writeWave(wavei$wave,'/Users/huzixin/Documents/soi/temp.wav')

#TransMusic

wavei$flow$time <- cumsum(wavei$flow$speed)-wavei$flow$speed[1]+1
wave2 <- wavei$flow %>%
  mutate(time2=floor(time/2)) %>%
  group_by(time=time2) %>%
  summarise(flow=flow[flow!=0][1]) %>%
  mutate(flow=ifelse(is.na(flow),0,flow)) %>%
  mutate(speed=1)

writeWave(processwav(wave2)$wave,'/Users/huzixin/Documents/soi/temp.wav')
