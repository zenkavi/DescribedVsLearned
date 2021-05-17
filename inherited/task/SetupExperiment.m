sca
clear
clc

global parcode
global numSessions
global fractals
global minReward
global maxReward

RandStream.setGlobalStream(RandStream('mt19937ar', 'seed', ...
    sum(100*clock)));

parcode = input('Participant code? ', 's');
numSessions = 5;
fractals = randsample(22, 2);
minReward = 80;
maxReward = 120;
