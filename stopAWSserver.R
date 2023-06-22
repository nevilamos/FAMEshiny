#disable function to stop or terminate  aws server generally would only be run at end of script
# if stopServer = NULL server continues
#if stopServer = "stop" it stops (still costs for storage but can be started again to get any save files or continue processing
#if stopServer = "terminate", server is terminated and anyting not saved off server will be permanently lost.)
stopAWSserver <- function(
  stopServer = NULL #"stop", "terminate"
) {
  try({if (is.null(stopServer)) {
    
  } else{
    if (stopServer == "stop") {
      unlink("index.html")
      system("wget http://169.254.169.254/latest/meta-data/instance-id/")
      myID <- readLines("index.html")[1]
      system(paste(
        "aws ec2 stop-instances --region ap-southeast-2 --instance-ids",
        myID)
      )
    } else{
      if (stopServer == "terminate") {
        unlink("index.html")
        system("wget http://169.254.169.254/latest/meta-data/instance-id/")
        myID <- readLines("index.html")[1]
        system(
          paste(
            "aws ec2 terminate-instances --region ap-southeast-2 --instance-ids",
            myID)
        )
      }
    }
  }
  }
  )
}