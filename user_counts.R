# read in csv
classifications = read.csv("20181115-sealSpotter_classifications_anonymised.csv")


# removes factors which caused it to slow down
classifications$imagefile = as.character(classifications$imagefile)


# create dataframe for classification ()
current_image = classifications$imagefile[1]
current_user = classifications$user_anonymised[1]
current_expert_status = classifications$expert[1]
count = 0
imagefile = c()
user_anonymised = c()
expert = c()
counts = c()
for(i in 1:nrow(classifications)){
  # if next user or image is different
  if( (classifications$user_anonymised[i] != current_user) | (classifications$imagefile[i] != current_image) ){
    # add information on the count
    imagefile = c(imagefile, current_image)
    user_anonymised = c(user_anonymised, current_user)
    expert = c(expert, current_expert_status)
    counts = c(counts, count)
    # initialise count information
    current_image = classifications$imagefile[i]
    current_user = classifications$user_anonymised[i]
    current_expert_status = classifications$expert[i]
    count = 0
  }
  # if the type is "zerocount"
  if(classifications$type[i] == "zerocount"){
    count = 0
  
  # if there is a seal identified
  } else if(classifications$type[i] != "comment"){
    count = count + 1
  }
}
# adds last classification
imagefile =  c(imagefile, current_image)
user_anonymised = c(user_anonymised, current_user)
expert = c(expert, current_expert_status)
counts = c(counts, count)
# put into dataframe
user_counts = data.frame(imagefile, user_anonymised, counts, expert)


# removes factors which caused it to slow down
user_counts$imagefile = as.character(user_counts$imagefile)

