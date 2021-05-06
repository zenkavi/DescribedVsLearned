library(R.matlab)

#Specify data path
data_path = '~/Downloads/GTavares_2017_arbitration/behavioral_data/'

#Get list of files in data_path
file_names = list.files(data_path)

#Field names for the matlab structure in case they aren't read in properly
field_names = c('currentDir', 'dataDir', 'parcode', 'fractals', 'session', 'fname', 'scan', 'debug', 'TR', 'numTrials', 'referenceValue', 'referenceProb', 'values', 'probs', 'numLotteries', 'probBundles', 'numRewardedTrials', 'randomWalkSigma', 'randomWalkLowBound', 'randomWalkUpBound', 'timeInstructions', 'timeRefLotteryPresentation', 'timeProbPresentation', 'timeRewardScreen', 'maxRT', 'possibleITIs', 'probFractalDraw', 'lotteryValue', 'lotteryProb', 'fractalLeftProb', 'fractalRightProb', 'fixedITI', 'fractalDraw', 'responseTime', 'reactionTime', 'responded', 'choiceLeft', 'reward', 'leftFractalReward', 'rightFractalReward', 'ITI', 'currTrialRemainder', 'onsetCross', 'onsetProbabilities', 'onsetStimulus', 'onsetReward', 'windowRect', 'choiceBoxSize', 'choiceBoxWidth', 'screenCenter', 'screenLowRight', 'screenLowLeft', 'screenUpRight', 'screenUpLeft', 'flipInterval', 'startTime', 'totalReward', 'endTimeSerial', 'endTime')

#Column names for the output data frame
df_names = c('subnum','session', 'trialNum', 'referenceValue', 'referenceProb', 'numRewardedTrials', 'probFractalDraw', 'lotteryValue', 'lotteryProb', 'fractalLeftProb', 'fractalRightProb', 'fractalDraw', 'reactionTime', 'responded', 'choiceLeft', 'reward', 'leftFractalReward', 'rightFractalReward','totalReward')

#Make empty data frame that will contain output
beh_data = data.frame(matrix(nrow = 60*length(file_names), ncol=length(df_names)))
names(beh_data) = df_names

#Function to check and complete session data with missing values
check_complete = function(listElement){
  
  if (length(c(listElement)) == 60){
    return(c(listElement))
  } else {
    na_reps = 60 - length(c(listElement))
    na_s = rep(NA, na_reps)
    return(c(listElement, na_s))
  }
}

#Loop through the files in the data path
#Read in and format each matlab file
#Put the reformatted session file into output file
print("Beginning reading data in...")
for (i in 1:length(file_names)){
  # print(paste0("Processing file: ", i, ' of ', length(file_names)))
  
  cur_mat = readMat(paste0(data_path, file_names[i]))
  names(cur_mat$data) = field_names
  
  cur_df = data.frame(matrix(nrow=60, ncol=length(df_names)))
  names(cur_df) = df_names
  
  cur_df$subnum = rep(gsub("[^\\d]+", "", cur_mat$data[['parcode']], perl=TRUE), 60)
  cur_df$session = rep(as.numeric(cur_mat$data[['session']]), 60)
  cur_df$trialNum = 1:nrow(cur_df)
  cur_df$referenceValue = rep(as.numeric(cur_mat$data[['referenceValue']]), 60)
  cur_df$referenceProb = rep(as.numeric(cur_mat$data[['referenceProb']]), 60)
  cur_df$numRewardedTrials = rep(as.numeric(cur_mat$data[['numRewardedTrials']]), 60)
  cur_df$probFractalDraw = check_complete(cur_mat$data[['probFractalDraw']])
  cur_df$lotteryValue = check_complete(cur_mat$data[['lotteryValue']])
  cur_df$lotteryProb = check_complete(cur_mat$data[['lotteryProb']])
  cur_df$fractalLeftProb = check_complete(cur_mat$data[['fractalLeftProb']])
  cur_df$fractalRightProb = check_complete(cur_mat$data[['fractalRightProb']])
  cur_df$fractalDraw = check_complete(cur_mat$data[['fractalDraw']])
  cur_df$reactionTime = check_complete(cur_mat$data[['reactionTime']])
  cur_df$responded = check_complete(cur_mat$data[['responded']])
  cur_df$choiceLeft = check_complete(cur_mat$data[['choiceLeft']]) 
  cur_df$reward = check_complete(cur_mat$data[['reward']])
  cur_df$leftFractalReward = check_complete(cur_mat$data[['leftFractalReward']])
  cur_df$rightFractalReward = check_complete(cur_mat$data[['rightFractalReward']])
  cur_df$totalReward = rep(as.numeric(cur_mat$data[['totalReward']]), 60)
  
  beh_data[((i-1)*60+1):(i*60),] = cur_df[,]
}

#Clean up
rm("cur_df", "cur_mat", "data_path", "df_names", "field_names", "file_names", "check_complete", "i")
print("Done!")

