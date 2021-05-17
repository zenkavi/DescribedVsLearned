library(rmatio)

#Specify data path
data_path = '/Users/zeynepenkavi/Downloads/GTavares_2017_arbitration/behavioral_data/'

#Get list of files in data_path
file_names = list.files(data_path)

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
  
  cur_mat = read.mat(paste0(data_path, file_names[i]))

  cur_df = data.frame(matrix(nrow=60, ncol=length(df_names)))
  names(cur_df) = df_names
  
  cur_df$subnum = rep(gsub("[^\\d]+", "", cur_mat$data[['parcode']], perl=TRUE), 60)
  cur_df$session = rep(as.numeric(cur_mat$data[['session']]), 60)
  cur_df$trialNum = 1:nrow(cur_df)
  cur_df$referenceValue = rep(as.numeric(cur_mat$data[['referenceValue']]), 60)
  cur_df$referenceProb = rep(as.numeric(cur_mat$data[['referenceProb']]), 60)
  cur_df$numRewardedTrials = rep(as.numeric(cur_mat$data[['numRewardedTrials']]), 60)
  cur_df$probFractalDraw = check_complete(cur_mat$data[['probFractalDraw']][[1]])
  cur_df$lotteryValue = check_complete(cur_mat$data[['lotteryValue']][[1]])
  cur_df$lotteryProb = check_complete(cur_mat$data[['lotteryProb']][[1]])
  cur_df$fractalLeftProb = check_complete(cur_mat$data[['fractalLeftProb']][[1]])
  cur_df$fractalRightProb = check_complete(cur_mat$data[['fractalRightProb']][[1]])
  cur_df$fractalDraw = check_complete(cur_mat$data[['fractalDraw']][[1]])
  cur_df$reactionTime = check_complete(cur_mat$data[['reactionTime']][[1]])
  cur_df$responded = check_complete(cur_mat$data[['responded']][[1]])
  cur_df$choiceLeft = check_complete(cur_mat$data[['choiceLeft']][[1]]) 
  cur_df$reward = check_complete(cur_mat$data[['reward']][[1]])
  cur_df$leftFractalReward = check_complete(cur_mat$data[['leftFractalReward']][[1]])
  cur_df$rightFractalReward = check_complete(cur_mat$data[['rightFractalReward']][[1]])
  cur_df$totalReward = rep(as.numeric(cur_mat$data[['totalReward']]), 60)
  
  beh_data[((i-1)*60+1):(i*60),] = cur_df[,]
}

#Clean up
rm("cur_df", "cur_mat", "data_path", "df_names", "file_names", "check_complete", "i")
print("Done!")

