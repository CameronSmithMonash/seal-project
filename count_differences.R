source("user_counts.R")


# calculate difference in counts for expert and non-expert classifications
images = unique(user_counts$imagefile) # list of unique images
image = c() # collumn of data frame containing images
mean_expert_count = c()
mean_non_expert_count = c()
difference = c()
positive_count = 0
negative_count = 0
for(img in images){
  if(img %in% expert$imagefile & img %in% non_expert$imagefile){
    image = c(image, img)
    img_expert_counts = expert$counts[ which(expert$imagefile == img)] # expert counts for this image
    img_non_expert_counts = non_expert$counts[ which(non_expert$imagefile == img)] # non expert counts for this image
    mean_expert_count = c(mean_expert_count, mean(img_expert_counts))
    mean_non_expert_count = c(mean_non_expert_count, mean(img_non_expert_counts))
    difference = c(difference, mean(img_expert_counts) - mean(img_non_expert_counts))
    if(mean(img_expert_counts) - mean(img_non_expert_counts) > 0){
      positive_count = positive_count + 1
    }else{
      if(mean(img_expert_counts) - mean(img_non_expert_counts) < 0){
        negative_count = negative_count + 1
      }
    }
  }  
}
count_differences = data.frame(image, mean_expert_count, mean_non_expert_count, difference)
non_zero_count_differences = count_differences[which(count_differences$mean_expert_count + count_differences$mean_non_expert_count != 0),] # same but excludes when both counts are 0


# hypothesis testing
# null hypothesis: mean difference <= 0 (non-experts over estimate and underestimate to the same degree)
# alternative hypothesis: mean difference > 0 (non - experts are more likely to underestimate)
t.test(x=count_differences$difference, mu=0, alternative="greater")

