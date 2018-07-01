# sending e-mail using R

# Package 'sendmailR'
# Package 'mail'



The sendmailR package makes it easy to send emails with attachments from the R command line. 

#load package
library("sendmailR")
help(package = 'sendmailR')

#use string formatting and your system info to format FROM address 
from <- sprintf("project name", Sys.info()[4])

to <- "<cliffordlong@ussco.com>"
subject <- "Test Email From R"

#create list with text of body as first element
#second list element is R object to attach using the mime_part() function 
body <- list("Email sent from R. Dataframe attached.",mime_part(data.frame(x=rnorm(1000),y=rnorm(1000)),name="output"))

sendmail(from, to, subject, body, control=list(smtpServer="ASPMX.L.GOOGLE.COM"))



#---------------------------------


sendEmail <-
  function(ema, name, subject, msgBody, deliverNow = TRUE)
  {
    require(RDCOMClient)
    
    ema <- paste("SMPT:", ema, sep="")   ## prepend protocol to address
    
    ## create an e-mail session 
    session <- COMCreate("Mapi.Session") 
    session$Logon()
    
    ## add a message to the outbox collection of messages
    outbox <- session[["Outbox"]]
    msg <- outbox[["Messages"]]$Add(subject, msgBody)
    
    ## add recipient's name  (TODO: addMultiple() or loop, if many recipients)
    msg[["Recipients"]]$Add(name, ema) 
    msg$Send()
    if(deliverNow)
      msg$DeliverNow()
    
    session$Logoff()   ## wrap up
  }


sendEmail(ema = "test e-mail from R", 
          name = "cliff",
          subject = "How to send Email from R using the RDCOMClient",
          msgBody = "here is the body of the message")


#---------------------------------

library(sendmailR)


#set working directory
setwd("C:/workingdirectorypath")

#####send plain email


# for SMS cell phone text via carrier e-mail
# http://www.emailtextmessages.com/
d.to = "8475021595@txt.att.net"  # Cliff
d.to = "6307303257@txt.att.net"  # Bart C.
d.to = "7703187539@vtext.com"  # Brian L.
d.to = "8153829984@txt.att.net"


d.to = c("jbarr@ussco.com", 
         "jeasto@ussco.com", 
         "bleutz@ussco.com",
         "mrogowski@ussco.com", 
         "ashah@ussco.com", 
         "aWahane@ussco.com", 
         "jzaworski@ussco.com", 
         "jmikes@ussco.com", 
         "fross@ussco.com", 
         "cliffordlong@ussco.com")

to.list = as.factor(c("cliffordlong@ussco.com", "cliffordlong@hotmail.com"))


for (i in 1:length(levels(to.list))){

	from <- "CliffordLong@ussco.com"
	# to <- "CliffordLong@ussco.com"
	subject <- "Email sent using R"
	body <- "Email body.\n Open the pod bay doors, HAL.\n I'm sorry, Dave. I'm afraid I can't do that."                     
	mailControl=list(smtpServer="ch2cas.ussco.com")

	sendmail(from=from, to=as.character(to.list[i]), subject=subject, msg=body, control=mailControl)

}  # end for-loop

# or for test
sendmail(from=from, to=d.to, subject=subject, msg=body, control=mailControl)



#######################################################################################
#####send same email with attachment

#needs full path if not in working directory
attachmentPath <- "subfolder/log.txt"

#same as attachmentPath if using working directory
attachmentName <- "log.txt"

#key part for attachments, put the body and the mime_part in a list for msg
attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
bodyWithAttachment <- list(body,attachmentObject)

sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)



##### END CODE #####
