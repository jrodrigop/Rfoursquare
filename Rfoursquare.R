# https://foursquare.com/oauth2/localhost?code=EIEOFWDGZNFG1W2EOUDSDODGRJBHFGRFTJ33ZN3LS1ORZVNL#_=_
# https://foursquare.com/oauth2/access_token?client_id=FQQ04T24I5FMPLVSJJSSNTW5GXCAHKQL12INNNJMZRA1KJIT&client_secret=O3QLBMI15KSGC1OW0LYNISDE3TNGTKE1MYGO5NQU5SQWNFFC&grant_type=authorization_code&redirect_uri=locahost&code=EIEOFWDGZNFG1W2EOUDSDODGRJBHFGRFTJ33ZN3LS1ORZVNL
# mi id de fq = 95052895
# DOCUMENTACION http://inlinedocs.r-forge.r-project.org/

# response errors: https://developer.foursquare.com/overview/responses
# Versioning:  https://developer.foursquare.com/overview/versioning
# v=20140806
# m=foursquare
# Accept-language=en (default), es, fr, de, it, ja, th, tr, ko, ru, pt, and id.
# add notice to policy https://developer.foursquare.com/overview/community

##################################################################
##################################################################
library(jsonlite)
library(httr)


# Default API version. Move this forward as the library is maintained and kept current
API_VERSION_YEAR  = '2014'
API_VERSION_MONTH = '08'
API_VERSION_DAY   = '06'
API_VERSION       = paste0(API_VERSION_YEAR,API_VERSION_MONTH,API_VERSION_DAY)


#https://foursquare.com/oauth2/authenticate?client_id=YOUR_CLIENT_ID&response_type=code&redirect_uri=YOUR_REGISTERED_REDIRECT_URI
#https://foursquare.com/oauth2/access_token?client_id=YOUR_CLIENT_ID&client_secret=YOUR_CLIENT_SECRET&grant_type=authorization_code&redirect_uri=YOUR_REGISTERED_REDIRECT_URI&code=CODE
AUTH_ENDPOINT = 'https://foursquare.com/oauth2/authenticate'
TOKEN_ENDPOINT = 'https://foursquare.com/oauth2/access_token'
API_ENDPOINT = 'https://api.foursquare.com/v2/'


initFoursquare <- function(CLIENT_ID, CLIENT_SECRET, ACCESS_TOKEN){
  CLIENT_ID <<- CLIENT_ID
  CLIENT_SECRET <<- CLIENT_SECRET
  ACCESS_TOKEN <<- ACCESS_TOKEN
}


fq_GET <- function(end_point,params,requiresActingUsr,userRestrictions){
  # make a list of parameters
  paramList = c()
  for(p in names(params))
    if(!is.na(params[p]))
      paramList[p]=params[p]
  paramList['v']               = API_VERSION
  if(requiresActingUsr == 'Yes'){ # programar el if no hay token, self funciona cuando es YES
    paramList['oauth_token']   = ACCESS_TOKEN    
  }else if(requiresActingUsr == 'No'){
    paramList['client_id']     = CLIENT_ID
    paramList['client_secret'] = CLIENT_SECRET
  }
  response=GET(url = paste0(API_ENDPOINT,end_point),query = as.list(paramList))
  if(headers(response)[["content-type"]] == "application/json; charset=utf-8"){
    response = fromJSON(content(response,'text'))  
    if(is.null(response$meta)){
      stop('No {\"meta\": { ... }} response, see https://developer.foursquare.com/overview/responses', call.=FALSE)  
    }else if(response$meta$code != 200){
      stop(paste0('[',response$meta$errorType,'] ',response$meta$errorDetail), call.=FALSE)
    }
  }else{
    stop("No JSON obtained", call.=FALSE)
  }  
  return(response$response)  
}

fq_POST <- function(end_point,params,requiresActingUsr,userRestrictions){
  # make a list of parameters
  paramList = c()  
  for(p in names(params))
    if(!is.na(params[p]))
      paramList[p]=params[p]
  paramList['v']               = API_VERSION
  if(requiresActingUsr == 'Yes'){ # programar el if no hay token, self funciona cuando es YES
    paramList['oauth_token']   = ACCESS_TOKEN    
  }else if(requiresActingUsr == 'No'){
    paramList['client_id']     = CLIENT_ID
    paramList['client_secret'] = CLIENT_SECRET
  }
  response=POST(url = paste0(API_ENDPOINT,end_point),query = as.list(paramList))
  if(headers(response)[["content-type"]] == "application/json; charset=utf-8"){
    response = fromJSON(content(response,'text'))  
    if(is.null(response$meta)){
      stop('No {\"meta\": { ... }} response, see https://developer.foursquare.com/overview/responses', call.=FALSE)  
    }else if(response$meta$code != 200){
      stop(paste0('[',response$meta$errorType,'] ',response$meta$errorDetail), call.=FALSE)
    }
  }else{
    stop("No JSON obtained", call.=FALSE)
  }  
  return(response$response)  }


# build POST request 
fq_GET_or_POST <- function(end_point,params,requiresActingUsr,userRestrictions){
    fq_POST(end_point,params,requiresActingUsr,userRestrictions)
}


##############################################################################################
##############################################################################################
users.users <- function(USER_ID="self",m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,""),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.requests <- function(m="swarm"){
  fq_GET(end_point="users/requests",params=as.list( environment() ),requiresActingUsr="Yes")
}
users.search <- function(phone=NULL, email=NULL, twitter=NULL, twitterSource=NULL, fbid=NULL, name=NULL,m="foursquare"){
  fq_GET_or_POST(end_point="users/search",params=as.list( environment() ),requiresActingUsr="Yes")
}
users.checkins <- function(USER_ID="self", limit=NULL, offset=NULL, sort=NULL, afterTimestamp=NULL, beforeTimestamp=NULL,m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/checkins"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.friends <- function(USER_ID="self", limit=NULL, offset=NULL,m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/friends"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.lists <- function(USER_ID="self", group=NULL, ll=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/lists"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.mayorships <- function(USER_ID="self",m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/mayorships"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.photos <- function(USER_ID="self", limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/photos"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.tips <- function(USER_ID="self", sort=NULL, ll=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/tips"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.venuehistory <- function(USER_ID="self", beforeTimestamp=NULL, afterTimestamp=NULL, categoryId=NULL,m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/venuehistory"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.venuelikes <- function(USER_ID="self", beforeTimestamp=NULL, afterTimestamp=NULL, categoryId=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/venuelikes"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.approve <- function(USER_ID="self",m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/approve"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.deny <- function(USER_ID="self",m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/deny"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.setpings <- function(USER_ID="self", value=NULL,m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/setpings"),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.unfriend <- function(USER_ID="self",m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/unfriend"),params=as.list( environment() ),requiresActingUsr="Yes")
}
#users.update <- function(photo	=NULL,m="swarm"){
#  fq_POST(end_point="users/update",params=as.list( environment() ),requiresActingUsr="Yes")
#}
venues.venues <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID),params=as.list( environment() ),requiresActingUsr="No")
}
venues.add <- function(name=NULL, address=NULL, crossStreet=NULL, city=NULL, state=NULL, zip=NULL, phone=NULL, twitter=NULL, ll=NULL, primaryCategoryId=NULL, description=NULL, url=NULL, ignoreDuplicates=NULL, ignoreDuplicatesKey=NULL,m="foursquare"){
  fq_POST(end_point="venues/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.categories <- function(m="foursquare"){
  fq_GET(end_point="venues/categories",params=as.list( environment() ),requiresActingUsr="No")
}
venues.explore <- function(ll=NULL, near=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, radius=NULL, section=NULL, query=NULL, limit=NULL, offset=NULL, novelty=NULL, friendVisits=NULL, time=NULL, day=NULL, venuePhotos=NULL, lastVenue=NULL, openNow=NULL, sortByDistance=NULL, price=NULL, saved=NULL, specials=NULL,m="foursquare"){
  fq_GET(end_point="venues/explore",params=as.list( environment() ),requiresActingUsr="No")
}
venues.managed <- function(limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point="venues/managed",params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.search <- function(ll=NULL, near=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, query=NULL, limit=NULL, intent=NULL, radius=NULL, sw=NULL, ne=NULL, categoryId=NULL, url=NULL, providerId=NULL, linkedId=NULL,m="foursquare"){
  fq_GET(end_point="venues/search",params=as.list( environment() ),requiresActingUsr="No")
}
venues.suggestcompletion <- function(ll=NULL, near=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, query=NULL, limit=NULL, radius=NULL, sw=NULL, ne=NULL,m="foursquare"){
  fq_GET(end_point="venues/suggestcompletion",params=as.list( environment() ),requiresActingUsr="No")
}
venues.timeseries <- function(venueId=NULL, startAt=NULL, endAt=NULL, fields=NULL,m="foursquare"){
  fq_GET(end_point="venues/timeseries",params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.trending <- function(ll=NULL, limit=NULL, radius=NULL,m="swarm"){
  fq_GET(end_point="venues/trending",params=as.list( environment() ),requiresActingUsr="No")
}
venues.events <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/events"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.herenow <- function(VENUE_ID=NULL, limit=NULL, offset=NULL,m="swarm"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/herenow"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.hours <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/hours"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.likes <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/likes"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.links <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/links"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.listed <- function(VENUE_ID=NULL, group=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/listed"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.menu <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/menu"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.nextvenues <- function(VENUE_ID=NULL,m="swarm"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/nextvenues"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.photos <- function(VENUE_ID=NULL, group=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/photos"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.similar <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/similar"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.stats <- function(VENUE_ID=NULL, startAt=NULL, endAt=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/stats"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.tips <- function(VENUE_ID=NULL, sort=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/tips"),params=as.list( environment() ),requiresActingUsr="No")
}
venues.claim <- function(visible=NULL,m="foursquare"){
  fq_POST(end_point="venues/claim",params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.dislike <- function(VENUE_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venues/",VENUE_ID,"/dislike"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.edit <- function(VENUE_ID=NULL, name=NULL, address=NULL, crossStreet=NULL, city=NULL, state=NULL, zip=NULL, phone=NULL, ll=NULL, primaryCategoryId=NULL, addCategoryIds=NULL, removeCategoryIds=NULL, twitter=NULL, description=NULL, url=NULL, storeId=NULL, hours=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venues/",VENUE_ID,"/edit"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.flag <- function(VENUE_ID=NULL, problem=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venues/",VENUE_ID,"/flag"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.like <- function(VENUE_ID=NULL, set=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venues/",VENUE_ID,"/like"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.proposeedit <- function(VENUE_ID=NULL, name=NULL, address=NULL, crossStreet=NULL, city=NULL, state=NULL, zip=NULL, phone=NULL, twitter=NULL, description=NULL, url=NULL, menuUrl=NULL, facebookUrl=NULL, ll=NULL, venuell=NULL, primaryCategoryId=NULL, addCategoryIds=NULL, removeCategoryIds=NULL, hours=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venues/",VENUE_ID,"/proposeedit"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.setrole <- function(VENUE_ID=NULL, userId=NULL, role=NULL, visible=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venues/",VENUE_ID,"/setrole"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venues.setsinglelocation <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/setsinglelocation"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.venuegroups <- function(GROUP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venuegroups/",GROUP_ID,"/venuegroups"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.add <- function(name=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point="venuegroups/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.delete <- function(GROUP_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/delete"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.list <- function(m="foursquare"){
  fq_GET(end_point="venuegroups/list",params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.timeseries <- function(GROUP_ID=NULL, startAt=NULL, endAt=NULL, fields=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venuegroups/",GROUP_ID,"/timeseries"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.addvenue <- function(GROUP_ID=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/addvenue"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.edit <- function(VENUEGROUP_ID=NULL, name=NULL, city=NULL, state=NULL, zip=NULL, phone=NULL, categoryId=NULL, twitter=NULL, description=NULL, url=NULL, hours=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",VENUEGROUP_ID,"/edit"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.removevenue <- function(GROUP_ID=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/removevenue"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.update <- function(GROUP_ID=NULL, name=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/update"),params=as.list( environment() ),requiresActingUsr="Yes")
}
checkins.checkins <- function(CHECKIN_ID=NULL, signature=NULL,m="swarm"){
  fq_GET(end_point=paste0("checkins/",CHECKIN_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
checkins.add <- function(venueId=NULL, eventId=NULL, shout=NULL, mentions=NULL, broadcast=NULL, ll=NULL, llAcc=NULL, alt=NULL, altAcc=NULL,m="swarm"){
  fq_POST(end_point="checkins/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
checkins.recent <- function(ll=NULL, limit=NULL, afterTimestamp=NULL,m="swarm"){
  fq_GET(end_point="checkins/recent",params=as.list( environment() ),requiresActingUsr="Yes")
}
checkins.likes <- function(CHECKIN_ID=NULL,m="swarm"){
  fq_GET(end_point=paste0("checkins/",CHECKIN_ID,"/likes"),params=as.list( environment() ),requiresActingUsr="No")
}
checkins.addcomment <- function(CHECKIN_ID=NULL, text=NULL, mentions=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/addcomment"),params=as.list( environment() ),requiresActingUsr="Yes")
}
checkins.addpost <- function(CHECKIN_ID=NULL, text=NULL, url=NULL, contentId=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/addpost"),params=as.list( environment() ),requiresActingUsr="Yes")
}
checkins.deletecomment <- function(CHECKIN_ID=NULL, commentId=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/deletecomment"),params=as.list( environment() ),requiresActingUsr="Yes")
}
checkins.like <- function(CHECKIN_ID=NULL, set=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/like"),params=as.list( environment() ),requiresActingUsr="Yes")
}
tips.tips <- function(TIP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID),params=as.list( environment() ),requiresActingUsr="No")
}
tips.add <- function(venueId=NULL, text=NULL, url=NULL, broadcast=NULL,m="foursquare"){
  fq_POST(end_point="tips/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
tips.likes <- function(TIP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID,"/likes"),params=as.list( environment() ),requiresActingUsr="No")
}
tips.listed <- function(TIP_ID=NULL, group=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID,"/listed"),params=as.list( environment() ),requiresActingUsr="No")
}
tips.saves <- function(TIP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID,"/saves"),params=as.list( environment() ),requiresActingUsr="No")
}
tips.flag <- function(TIP_ID=NULL, comment=NULL, problem=NULL,m="foursquare"){
  fq_POST(end_point=paste0("tips/",TIP_ID,"/flag"),params=as.list( environment() ),requiresActingUsr="Yes")
}
tips.like <- function(TIP_ID=NULL, set=NULL,m="foursquare"){
  fq_POST(end_point=paste0("tips/",TIP_ID,"/like"),params=as.list( environment() ),requiresActingUsr="Yes")
}
tips.unmark <- function(TIP_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("tips/",TIP_ID,"/unmark"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.lists <- function(USER_ID=NULL, defaultList="todos", LIST_ID=NULL, limit=NULL, offset=NULL, llBounds=NULL, categoryId=NULL, sort=NULL,m="foursquare"){
  if(!is.null(USER_ID) & !is.null(LIST_ID)){
    stop("Choose either LIST_ID for a user-created or followed list.
         OR USER_ID=XXX and defaultList=c(\"todos\",\"tips\"), not both")
  }else if(!is.null(USER_ID)){    
    fq_GET(end_point=paste0("lists/",USER_ID,"/",defaultList), params = list(), requiresActingUsr="Yes")    
  }else if(!is.null(LIST_ID)){
    fq_GET(end_point=paste0("lists/",LIST_ID),params=environment(),requiresActingUsr="No")  
  }else{
    stop("Choose either LIST_ID for a user-created or followed list.
         OR USER_ID=XXX and defaultList=c(\"todos\",\"tips\")")
  }  
}
lists.add <- function(name=NULL, description=NULL, collaborative=NULL, photoId=NULL,m="foursquare"){
  fq_POST(end_point="lists/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.followers <- function(LIST_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/followers"),params=as.list( environment() ),requiresActingUsr="No")
}
lists.items <- function(LIST_ID=NULL, ITEM_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID, "/", ITEM_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.saves <- function(LIST_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/saves"),params=as.list( environment() ),requiresActingUsr="No")
}
lists.suggestphoto <- function(LIST_ID=NULL, itemId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/suggestphoto"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.suggesttip <- function(LIST_ID=NULL, itemId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/suggesttip"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.suggestvenues <- function(LIST_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/suggestvenues"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.additem <- function(LIST_ID=NULL, venueId=NULL, url=NULL, tipId=NULL, listId=NULL, itemId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/additem"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.deleteitem <- function(LIST_ID=NULL, itemId=NULL, venueId=NULL, tipId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/deleteitem"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.follow <- function(LIST_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/follow"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.moveitem <- function(LIST_ID=NULL, itemId=NULL, beforeId=NULL, afterId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/moveitem"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.share <- function(LIST_ID=NULL, broadcast=NULL, message=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/share"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.unfollow <- function(LIST_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/unfollow"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.update <- function(LIST_ID=NULL, name=NULL, description=NULL, collaborative=NULL, photoId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/update"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.updateitem <- function(LIST_ID=NULL, itemId=NULL, tipId=NULL, text=NULL, url=NULL, photoId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/updateitem"),params=as.list( environment() ),requiresActingUsr="Yes")
}
updates.updates <- function(UPDATE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("updates/",UPDATE_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
updates.notifications <- function(limit=NULL,m="foursquare"){
  fq_GET(end_point="updates/notifications",params=as.list( environment() ),requiresActingUsr="Yes")
}
updates.marknotificationsread <- function(highWatermark=NULL,m="foursquare"){
  fq_POST(end_point="updates/marknotificationsread",params=as.list( environment() ),requiresActingUsr="Yes")
}
#photos.photos <- function(PHOTO_ID=NULL,m="foursquare"){
#  fq_GET(end_point=paste0("photos/",PHOTO_ID,"/photos"),params=as.list( environment() ),requiresActingUsr="Yes")
#}
#photos.add <- function(checkinId=NULL, tipId=NULL, venueId=NULL, pageId=NULL, broadcast=NULL, public=NULL, ll=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, postUrl=NULL, postContentId=NULL, postText=NULL,m="foursquare"){
#  fq_POST(end_point="photos/add",params=as.list( environment() ),requiresActingUsr="Yes")
#}
settings.settings <- function(SETTING_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("settings/",SETTING_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
settings.all <- function(m="foursquare"){
  fq_GET(end_point="settings/all",params=as.list( environment() ),requiresActingUsr="Yes")
}
settings.set <- function(SETTING_ID=NULL, value=NULL,m="foursquare"){
  fq_POST(end_point=paste0("settings/",SETTING_ID,"/set"),params=as.list( environment() ),requiresActingUsr="Yes")
}
specials.specials <- function(SPECIAL_ID=NULL, venueId=NULL, userId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("specials/",SPECIAL_ID),params=as.list( environment() ),requiresActingUsr="No")
}
specials.add <- function(name=NULL, text=NULL, finePrint=NULL, count1=NULL, type=NULL, offerId=NULL, cost=NULL,m="foursquare"){
  fq_POST(end_point="specials/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
specials.list <- function(venueId=NULL, status=NULL,m="foursquare"){
  fq_GET(end_point="specials/list",params=as.list( environment() ),requiresActingUsr="Yes")
}
specials.search <- function(ll=NULL, radius=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, limit=NULL,m="foursquare"){
  fq_GET(end_point="specials/search",params=as.list( environment() ),requiresActingUsr="No")
}
specials.flag <- function(ID=NULL, venueId=NULL, problem=NULL, text=NULL,m="foursquare"){
  fq_POST(end_point=paste0("specials/",ID,"/flag"),params=as.list( environment() ),requiresActingUsr="Yes")
}
events.events <- function(EVENT_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("events/",EVENT_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
events.categories <- function(m="foursquare"){
  fq_GET(end_point="events/categories",params=as.list( environment() ),requiresActingUsr="No")
}
events.search <- function(domain=NULL, eventId=NULL, participantId=NULL,m="foursquare"){
  fq_GET(end_point="events/search",params=as.list( environment() ),requiresActingUsr="No")
}
events.add <- function(venueId=NULL, name=NULL, startAt=NULL, endAt=NULL,m="foursquare"){
  fq_POST(end_point="events/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
pages.add <- function(name=NULL,m="foursquare"){
  fq_POST(end_point="pages/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
pages.managing <- function(m="foursquare"){
  fq_GET(end_point="pages/managing",params=as.list( environment() ),requiresActingUsr="Yes")
}
pages.access <- function(USER_ID="self",m="foursquare"){
  fq_GET(end_point=paste0("pages/",USER_ID,"/access"),params=as.list( environment() ),requiresActingUsr="Yes")
}
pages.similar <- function(USER_ID="self", limit=NULL, offset=NULL, includeFollowing=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pages/",USER_ID,"/similar"),params=as.list( environment() ),requiresActingUsr="Yes")
}
pages.timeseries <- function(PAGE_ID=NULL, startAt=NULL, endAt=NULL, fields=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pages/",PAGE_ID,"/timeseries"),params=as.list( environment() ),requiresActingUsr="Yes")
}
pages.venues <- function(PAGE_ID=NULL, ll=NULL, radius=NULL, sw=NULL, ne=NULL, offset=NULL, limit=NULL, storeId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pages/",PAGE_ID,"/venues"),params=as.list( environment() ),requiresActingUsr="No")
}
#pages.follow <- function(USER_ID="self", set=NULL,m="foursquare"){
#  fq_POST(end_point=paste0("pages/",USER_ID,"/follow"),params=as.list( environment() ),requiresActingUsr="Yes")
#}
pageupdates.pageupdates <- function(UPDATE_ID=NULL, limit=NULL, ll=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pageupdates/",UPDATE_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
pageupdates.add <- function(pageId=NULL, groupId=NULL, venueId=NULL, shout=NULL, photoId=NULL, broadcast=NULL,m="foursquare"){
  fq_POST(end_point="pageupdates/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
pageupdates.list <- function(m="foursquare"){
  fq_GET(end_point="pageupdates/list",params=as.list( environment() ),requiresActingUsr="Yes")
}
pageupdates.delete <- function(UPDATE_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("pageupdates/",UPDATE_ID,"/delete"),params=as.list( environment() ),requiresActingUsr="Yes")
}
pageupdates.like <- function(UPDATE_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("pageupdates/",UPDATE_ID,"/like"),params=as.list( environment() ),requiresActingUsr="Yes")
}
