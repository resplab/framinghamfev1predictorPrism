model_run<-function(model_input = NULL)
{

    input<-unflatten_list(model_input)
    # predictors <- data.frame(fev1_0=model_input$fev1_0,
    #                          fvc_0=model_input$fvc_0,
    #                          age=model_input$age,
    #                          triglycerides=model_input$triglycerides,
    #                          hematocrit=model_input$hematocrit,
    #                          albumin=model_input$albumin,
    #                          globulin=model_input$globulin,
    #                          ALP=model_input$ALP,
    #                          WBC=model_input$WBC,
    #                          QRS_intv=model_input$QRS_intv,
    #                          wine=model_input$wine,
    #                          beer=model_input$beer,
    #                          cocktail=model_input$cocktail,
    #                          height=model_input$height,
    #                          smoke_year=model_input$smoke_year,
    #                          daily_cigs=model_input$daily_cigs,
    #                          sex=model_input$sex,
    #                          broncho=model_input$broncho,
    #                          dyspnea_exc=model_input$dyspnea_exc,
    #                          night_sym=model_input$night_sym)
    results <- framinghamPredictor::make_predictions(resp_var=model_input$resp_var,
                                predictors=model_input$predictors)

    return(as.list(results))
}


get_default_input <- function() {

  model_input <- list(resp_var='fev1', predictors=framinghamPredictor::sample_predictors)

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
