iap_relate = function(df,yobs,ypred){

  class_obs = class(df %>% pull({{yobs}}))
  class_pred = class(df %>% pull({{ypred}}))

  if(class_obs != class_pred){
    rlang::abort(message=paste("Predicted and observed must either both be continuous or both be factor variables. The class of yobs is", class_obs,"and the class of ypred is", class_pred,"."))
  }

  pred_var = deparse(substitute(ypred))
  obs_var = deparse(substitute(yobs))

  if(is.numeric(df %>% pull({{yobs}}))){
    form = as.formula(paste(obs_var, "~ s(", pred_var,")"))
    rel_mod = gam(form,data=df)
  }else{

  }



  return(rel_mod)
}
