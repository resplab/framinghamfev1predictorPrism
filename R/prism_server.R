## v0.3.0 2019-05-27

get_my_name<-function()
{
  x<-getPackageName()
  return(x)
}


thisSession<-new.env()

thisSession$MODE_REQUIRE_API_KEY=TRUE;
thisSession$MODE_REQUIRE_SESSION=FALSE;
thisSession$MODE_REQUIRE_SESSION_DATA=FALSE;

thisSession$LONG_RUN_STATUS_READY<-0
thisSession$LONG_RUN_STATUS_DONE<-1
thisSession$LONG_RUN_STATUS_ERROR<- -1

thisSession$MODEL_DESCRIPTION<-paste0("This is ",get_my_name()," - PRISM enabled!")
thisSession$MODEL_VERSION<-paste(packageVersion(get_my_name()))


#FOR ONE SHOT JSON CALL!
#Can authenticate the user either by API key or by a session_id.
#' @export
gateway<-function(...)
{
  arguments=list(...)
  func<-arguments$func

  session_id<-arguments$session_id
  if(is.null(session_id)) session_id=""

  session_id<<-session_id

  arguments$func<-NULL
  arguments$api_key<-NULL
  arguments$session_id<-NULL


  if(length(arguments)==0) {
    out<-eval(parse(text=paste(func,"()")))
  }
  else {
    out<-do.call(func, args = arguments)
  }

  return(jsonlite::toJSON(out))
}


#' @export
prism_model_run<-function(model_input=NULL)
{
  session_id<<-generate_session_id()
  thisSession$session_id = session_id
  return(list(results=model_run(model_input), session_id=session_id))
}

#In API-based use without session ids this might seem a bit redundant (it will not be required). But still good to check model availability
connect_to_model<-function(api_key="")
{
  model_name<-environmentName(environment(connect_to_model))
  out<-list(error_code=0,session_id="",version="",description="")


  if(thisSession$MODE_REQUIRE_SESSION)
  {
    session_id<-generate_session_id()
    out$session_id<-session_id
  }

  out$version<-thisSession$MODEL_VERSION
  out$description<-thisSession$MODEL_DESCRIPTION
  return(out)
}


generate_session_id<-function()
{
  id<-paste(c(sample(letters,1) , sample(c(letters,0:9),9,TRUE)),collapse="")
  return(id)
}


set_var<-function(variable,value)
{
  .GlobalEnv[[variable]]<-value
}


get_var<-function(variable)
{
  return(.GlobalEnv[[variable]])
}

