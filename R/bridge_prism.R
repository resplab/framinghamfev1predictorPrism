model_run<-function(model_input = NULL)
{

  input<-unflatten_list(model_input)
  # replace the function below with main model function takes model inputs and returns the output.
  # for example, for bode package we will have:
  # results <- bode              (FEV1                   =model_input$FEV1,
  #                               mMRC                   =model_input$mMRC,
  #                               BMI                    =model_input$BMI,
  #                               walk                   =model_input$walk)
  #
  # for cfmortality package, we will have:
  # results <- predictcfmortality(age                    =model_input$age,
  #                               male                   =model_input$male,
  #                               fvc                    =model_input$fvc,
  #                               fev1                   =model_input$fev1,
  #                               fev1LastYear           =model_input$fev1LastYear,
  #                               bcepacia               =model_input$bcepacia,
  #                               underweight            =model_input$underweight,
  #                               nHosp                  =model_input$nHosp,
  #                               pancreaticInsufficient =model_input$pancreaticInsufficient,
  #                               CFRelatedDiabetes      =model_input$CFRelatedDiabetes,
  #                               ageAtDiagnosis         =model_input$ageAtDiagnosis        )

  return(as.list(results))
}


get_default_input <- function() {

  predictors <- data.frame(fev1_0=1,
                           fvc_0=3,
                           age=50,
                           triglycerides=NA,
                           hematocrit=NA,
                           albumin=NA,
                           globulin=NA,
                           ALP=NA,
                           WBC=NA,
                           QRS_intv=NA,
                           wine=NA,
                           cocktail=NA,
                           height=180,
                           smoke_year=NA,
                           daily_cigs=NA,
                           sex="female",
                           broncho="",
                           dyspnea_exc="",
                           night_sym="",
                           alcohol_indx=NA)

  fev1_full_file_name = paste("./",paste(file_name(), collapse=""), "-fev1", ".rds",sep="")
  fev1_lmer_function_output <- readRDS(fev1_full_file_name)

  model_input <- list(resp_var='fev1', lmfin=fev1_lmer_function_output, predictors=predictors)

  return((flatten_list(model_input)))
}


#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}



#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()

  nms<-names(lst)

  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }

  return(out)
}
