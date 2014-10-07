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

#' User
#' 
#'  Returns profile information for a given user.
#' 
#' Returns profile information for a given user, including
#'  selected badges and mayorships. The web profile for a user
#'	is visible at https://foursquare.com/user/USER_ID  If the
#'	user is a friend, contact information, Facebook ID, and
#'	Twitter handle and the user's last checkin may also be
#'	present.  In addition, the pings field will indicate
#'	whether checkins from this user will trigger a ping
#'	(notifications to mobile devices). This setting can be
#'	changed via setpings.  Note that this setting is overriden
#'	if pings is false in settings (no pings will be sent, even
#'	if this user is set to true).
#' 	Note: This function requires acting user
#' 
#' @param USER_ID Identity of the user to get details for. Pass
#'	self to get details of the acting user.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return user A user.
#' 
#' @examples
#' users.users(USER_ID="HZXXY3")
users.users <- function(USER_ID="self",m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,""),params=as.list( environment() ),requiresActingUsr="Yes")
}
users.requests <- function(m="swarm"){
  fq_GET(end_point="users/requests",params=as.list( environment() ),requiresActingUsr="Yes")
}


#' Find users
#' 
#'  Helps a user locate friends. 
#' 
#' Helps a user locate friends.
#' 	Note: This function requires acting user
#' 
#' @param phone A comma-delimited list of phone numbers to look
#'	for.
#' @param email A comma-delimited list of email addresses to
#'	look for.
#' @param twitter A comma-delimited list of Twitter handles to
#'	look for.
#' @param twitterSource A single Twitter handle. Results will
#'	be users that this handle follows on Twitter who use
#'	Foursquare.
#' @param fbid A comma-delimited list of Facebook ID's to look
#'	for.
#' @param name A single string to search for in users' names.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return results An array of compact user objects with
#'	Twitter or Facebook information and friend status. An array
#'	of unmatched search terms grouped into subarrays
#'	corresponding to the term's parameter.
#' 
#' @examples
#' users.search(phone="91712344567,123450",
#'	email="foo@@bar.com,alice@@baz.com",
#'	twitter="dens,sesamestreet", twitterSource="krave",
#'	fbid="123,456", name="Fred Fredrickson")
users.search <- function(phone=NULL, email=NULL, twitter=NULL, twitterSource=NULL, fbid=NULL, name=NULL,m="foursquare"){
  fq_GET_or_POST(end_point="users/search",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Checkins by a user
#' 
#'  Returns a history of checkins for the authenticated user.  
#' 
#' Returns a history of checkins for the authenticated user. 
#'	Note: We highly discourage developers from polling this
#'	endpoint to learn of new check-ins or to keep their users'
#'	check-in history synced. Using the real-time API is a much
#'	better solution.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID For now, only self is supported
#' @param limit Number of results to return, up to 250.
#' @param offset The number of results to skip. Used to page
#'	through results.
#' @param sort How to sort the returned checkins. Can be
#'	newestfirst or oldestfirst.
#' @param afterTimestamp Retrieve the first results to follow
#'	these seconds since epoch. This should be useful for paging
#'	forward in time, or when polling for changes. To avoid
#'	missing results when polling, we recommend subtracting
#'	several seconds from the last poll time and then
#'	de-duplicating.
#' @param beforeTimestamp Retrieve the first results prior to
#'	these seconds since epoch. Useful for paging backward in
#'	time.
#' @param m Accepts values of "swarm"
#' 
#' @return checkins A count and items of check-ins.
#' 
#' @examples
#' users.checkins(USER_ID="self", limit="100", offset="100",
#'	sort="newestfirst", afterTimestamp="1279044824",
#'	beforeTimestamp="1279044824")
users.checkins <- function(USER_ID="self", limit=NULL, offset=NULL, sort=NULL, afterTimestamp=NULL, beforeTimestamp=NULL,m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/checkins"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' List friends
#' 
#'  Returns an array of a user's friends. 
#' 
#' Returns an array of a user's friends.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID Identity of the user to get friends of. Pass
#'	self to get friends of the acting user.
#' @param limit Number of results to return, up to 500.
#' @param offset Used to page through results.
#' @param m Accepts values of "swarm"
#' 
#' @return friends A count and items of compact user objects.
#' 
#' @examples
#' users.friends(USER_ID="HZXXY3Y", limit="100", offset="100")
users.friends <- function(USER_ID="self", limit=NULL, offset=NULL,m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/friends"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Lists
#' 
#'  A User's Lists. 
#' 
#' A User's Lists.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID Identity of the user to get lists for. Pass
#'	self to get lists of the acting user.
#' @param group can be created (lists created by this user),
#'	edited (other people's lists this user has edited),
#'	followed (lists this user follows), friends (lists from
#'	this user's friends), and suggested (lists relevant to the
#'	user's current location).
#' @param ll Location of the user, required in order to receive
#'	the suggested group.
#' @param limit Number of results to return, up to 200.
#' @param offset The number of results to skip. Used to page
#'	through results.
#' @param m Accepts values of "foursquare"
#' 
#' @return lists If group is specified, contains a count and
#'	items of lists. If no group is specified, it contains a
#'	groups array containing elements, each with type, name, an
#'	optional count and optional items. The type field can be
#'	passed in as a group to get more elements. If present,
#'	count represents the total number of elements. If not
#'	specified, there are a potentially unbounded number of
#'	elements. items contains a sample of the items.
#' 
#' @examples
#' users.lists(USER_ID="HZXXY3Y", group="edited",
#'	ll="40.7,-74", limit="100", offset="100")
users.lists <- function(USER_ID="self", group=NULL, ll=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/lists"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' List mayorships
#' 
#'  Returns a user's mayorships. 
#' 
#' Returns a user's mayorships.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID Identity of the user to get mayorships for.
#'	Pass self to get friends of the acting user.
#' @param m Accepts values of "swarm"
#' 
#' @return mayorships A count and items of objects which
#'	currently only contain compact venue objects.
#' 
#' @examples
#' users.mayorships(USER_ID="HZXXY3Y")
users.mayorships <- function(USER_ID="self",m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/mayorships"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Photos from a User
#' 
#'  Returns photos from a user. 
#' 
#' Returns photos from a user.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID For now, only self is supported.
#' @param limit Number of results to return, up to 500.
#' @param offset Used to page through results.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return photos A count and items of photos.
#' 
#' @examples
#' users.photos(USER_ID="self", limit="100", offset="100")
users.photos <- function(USER_ID="self", limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/photos"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Tips from a User
#' 
#'  Returns tips from a user.  This endpoint is deprecated.
#' 
#' Returns tips from a user.  This endpoint is deprecated. Use
#'	/lists/USER_ID/tips instead.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID Identity of the user to get tips from. Pass
#'	self to get tips of the acting user.
#' @param sort One of recent, nearby, or popular. Nearby
#'	requires geolat and geolong to be provided.
#' @param ll Latitude and longitude of the user's location.
#' @param limit Number of results to return, up to 500.
#' @param offset Used to page through results.
#' @param m Accepts values of "foursquare"
#' 
#' @return tips A count and items of tips.
#' 
#' @examples
#' users.tips(USER_ID="HZXXY3Y", sort="recent", ll="33.7,44.2",
#'	limit="100", offset="100")
users.tips <- function(USER_ID="self", sort=NULL, ll=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/tips"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Venues visited by a user
#' 
#'  Returns a list of all venues visited by the specified user.
#' 
#' Returns a list of all venues visited by the specified user,
#'	along with how many visits and when they were last there.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID For now, only self is supported
#' @param beforeTimestamp Seconds since epoch.
#' @param afterTimestamp Seconds after epoch.
#' @param categoryId Limits returned venues to those in this
#'	category. If specifying a top-level category, all
#'	sub-categories will also match the query.
#' @param m Accepts values of "swarm"
#' 
#' @return venues A count and items of objects containing a
#'	beenHere count and venue compact venues.
#' 
#' @examples
#' users.venuehistory(USER_ID="self",
#'	beforeTimestamp="123456789", afterTimestamp="123456789",
#'	categoryId="asad13242ljla")
users.venuehistory <- function(USER_ID="self", beforeTimestamp=NULL, afterTimestamp=NULL, categoryId=NULL,m="swarm"){
  fq_GET(end_point=paste0("users/",USER_ID,"/venuehistory"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Venues liked by a user
#' 
#'  Returns a list of venues liked by the specified user.
#' 
#' Returns a list of venues liked by the specified user
#' 	Note: This function requires acting user
#' 
#' @param USER_ID User ID or self
#' @param beforeTimestamp Seconds since epoch.
#' @param afterTimestamp Seconds since epoch.
#' @param categoryId Limits returned venues to those in this
#'	category. If specifying a top-level category, all
#'	sub-categories will also match the query.
#' @param limit Number of results to return.
#' @param offset Used to page through results.
#' @param m Accepts values of "foursquare"
#' 
#' @return venues A count and items of objects containing a
#'	beenHere count and venue compact venues.
#' 
#' @examples
#' users.venuelikes(USER_ID="self",
#'	beforeTimestamp="123456789", afterTimestamp="123456789",
#'	categoryId="asad13242ljla", limit="100", offset="100")
users.venuelikes <- function(USER_ID="self", beforeTimestamp=NULL, afterTimestamp=NULL, categoryId=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("users/",USER_ID,"/venuelikes"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Approve a friend request
#' 
#'  Approves a pending friend request from another user. 
#' 
#' Approves a pending friend request from another user.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID required The user ID of a pending friend.
#' @param m Accepts values of "swarm"
#' 
#' @return user A user object for the approved user.
#' 
#' @examples
#' users.approve(USER_ID="HZXXY3Y")
users.approve <- function(USER_ID="self",m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/approve"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Deny a friend request
#' 
#'  Denies a pending friend request from another user. 
#' 
#' Denies a pending friend request from another user.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID required The user ID of a pending friend.
#' @param m Accepts values of "swarm"
#' 
#' @return user A user object for the denied user.
#' 
#' @examples
#' users.deny(USER_ID="HZXXY3Y")
users.deny <- function(USER_ID="self",m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/deny"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Set whether to receive pings about a user
#' 
#'  Changes whether the acting user will receive pings.
#' 
#' Changes whether the acting user will receive pings (phone
#'	notifications) when the specified user checks in.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID required The user ID of a friend.
#' @param value required True or false.
#' @param m Accepts values of "swarm"
#' 
#' @return user A user object for the user.
#' 
#' @examples
#' users.setpings(USER_ID="HZXXY3", value="false")
users.setpings <- function(USER_ID="self", value=NULL,m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/setpings"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Remove a Friend
#' 
#'  Cancels relationship of the acting user.
#' 
#' Cancels any relationship between the acting user and the
#'	specified user.  Removes a friend, unfollows a celebrity,
#'	or cancels a pending friend request.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID Identity of the user to unfriend.
#' @param m Accepts values of "swarm"
#' 
#' @return user A user.
#' 
#' @examples
#' users.unfriend(USER_ID="HZXXY3")
users.unfriend <- function(USER_ID="self",m="swarm"){
  fq_POST(end_point=paste0("users/",USER_ID,"/unfriend"),params=as.list( environment() ),requiresActingUsr="Yes")
}

##' Update user's photo
##' 
##'  Updates the user's profile photo. 
##' 
##' Updates the user's profile photo.
##' 	Note: This function requires acting user
##' 
##' @param photo	 Photo under 100KB in multipart MIME encoding
##'	with content type image/jpeg, image/gif, or image/png.
##' @param m Accepts values of "swarm"
##' 
##' @return user The current user object.
##' 
##' @examples
##' users.update(photo	="file.jpg")
#users.update <- function(photo	=NULL,m="swarm"){
#  fq_POST(end_point="users/update",params=as.list( environment() ),requiresActingUsr="Yes")
#}

#' Venue Detail
#' 
#'  Gives details about a venue.
#' 
#' Gives details about a venue, including location, mayorship,
#'	tags, tips, specials, and category.  Authenticated users
#'	will also receive information about who is here now.  If
#'	the venue ID given is one that has been merged into another
#'	"master" venue, the response will show data about the
#'	"master" instead of giving you an error.
#' 
#' @param VENUE_ID required ID of venue to retrieve
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return venue A complete venue.
#' 
#' @examples
#' venues.venues(VENUE_ID="A9ABCD")
venues.venues <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID),params=as.list( environment() ),requiresActingUsr="No")
}

#' Add a Venue
#' 
#'  Allows Foursquare users to add a new venue.
#' 
#' Allows Foursquare users to add a new venue.   All fields are
#'	optional, except for ll and name.  Category ID’s Although
#'	optional, we strongly recommend that developers pass in the
#'	primaryCategoryId parameter to assign the new venue a
#'	category. The resulting venues become more meaningful
#'	within Foursquare and are more easily searchable by other
#'	users. We recommend that applications show their users our
#'	category hierarchy (obtained from venues/categories) and
#'	allow them to choose something suitable.  Duplicate Venue
#'	Errors The method may return an HTTP 409 error if the new
#'	venue looks like a duplicate of an existing venue. This
#'	error response will include two useful values:
#'	candidateDuplicateVenues and ignoreDuplicatesKey.   In this
#'	situation, you can either:  Use one of the
#'	candidateDuplicateVenues included in the response of the
#'	409 error. This will not create a new venue. Ignore
#'	duplicates and force the addition of a new venue by
#'	resubmitting the same venue add request with two additional
#'	parameters: ignoreDuplicates set to true and
#'	ignoreDuplicatesKey set to the value from the earlier error
#'	response.   The decision to ignore duplicates should be up
#'	to your users—developers should not always just pass
#'	ignoreDuplicates=true.  Adding Venues Without
#'	Authenticating Users First This endpoint generally requires
#'	you to authenticate Foursquare users before you can add
#'	venues on their behalf. In some cases, we’ll make
#'	exceptions and allow applications to create new venues
#'	without authenticating any users. If you’re interested in
#'	this feature, please contact api@@foursquare.com.
#' 	Note: This function requires acting user
#' 
#' @param name required the name of the venue
#' @param address The address of the venue.
#' @param crossStreet The nearest intersecting street or
#'	streets.
#' @param city The city name where this venue is.
#' @param state The nearest state or province to the venue.
#' @param zip The zip or postal code for the venue.
#' @param phone The phone number of the venue.
#' @param twitter The twitter handle of the venue.
#' @param ll required Latitude and longitude of the venue, as
#'	accurate as is known.
#' @param primaryCategoryId The ID of the category to which you
#'	want to assign this venue.
#' @param description A freeform description of the venue, up
#'	to 160 characters.
#' @param url The url of the homepage of the venue.
#' @param ignoreDuplicates A boolean flag telling the server to
#'	ignore duplicates and force the addition of this venue.
#' @param ignoreDuplicatesKey Required if ignoreDuplicates is
#'	true. This key will be available in the response of the
#'	HTTP 409 error of the first (failed) attempt to add venue.
#' @param m Accepts values of "foursquare"
#' 
#' @return venue The venue that was just created.
#' 
#' @examples
#' venues.add(name="Habana Outpost", address="1313 Mockingbird
#'	Lane", crossStreet="at Fulton St", city="New York",
#'	state="New York", zip="AE1234", phone="00 01 23 1234",
#'	twitter="eathabana", ll="44.3,37.2",
#'	primaryCategoryId="4bf58dd8d48988d1d4941735",
#'	description="We are a family owned and operated business
#'	with our customers' satisfaction ... ",
#'	url="http://www.mercurylounge.com",
#'	ignoreDuplicates="true",
#'	ignoreDuplicatesKey="bb29f248166648fb51d8d92b1740444c")
venues.add <- function(name=NULL, address=NULL, crossStreet=NULL, city=NULL, state=NULL, zip=NULL, phone=NULL, twitter=NULL, ll=NULL, primaryCategoryId=NULL, description=NULL, url=NULL, ignoreDuplicates=NULL, ignoreDuplicatesKey=NULL,m="foursquare"){
  fq_POST(end_point="venues/add",params=as.list( environment() ),requiresActingUsr="Yes")
}

venues.categories <- function(m="foursquare"){
  fq_GET(end_point="venues/categories",params=as.list( environment() ),requiresActingUsr="No")
}

#' Explore Recommended and Popular Venues
#' 
#'  Returns a list of recommended venues near the current loc.
#' 
#' Returns a list of recommended venues near the current
#'	location.  If authenticated, the method will potentially
#'	personalize the ranking based on you and your friends. If
#'	you do not authenticate, you will not get this
#'	personalization.  This endpoint is part of the venues API.
#' 
#' @param ll required unless near is provided. Latitude and
#'	longitude of the user's location.
#' @param near required unless ll is provided. A string naming
#'	a place in the world. If the near string is not geocodable,
#'	returns a failed_geocode error. Otherwise, searches within
#'	the bounds of the geocode and adds a geocode object to the
#'	response.
#' @param llAcc Accuracy of latitude and longitude, in meters.
#' @param alt Altitude of the user's location, in meters.
#' @param altAcc Accuracy of the user's altitude, in meters.
#' @param radius Radius to search within, in meters.  If radius
#'	is not specified, a suggested radius will be used based on
#'	the density of venues in the area.
#' @param section One of food, drinks, coffee, shops, arts,
#'	outdoors, sights, trending or specials, nextVenues (venues
#'	frequently visited after a given venue), or topPicks (a mix
#'	of recommendations generated without a query from the
#'	user). Choosing one of these limits results to venues with
#'	the specified category or property.
#' @param query A term to be searched against a venue's tips,
#'	category, etc. The query parameter has no effect when a
#'	section is specified.
#' @param limit Number of results to return, up to 50.
#' @param offset Used to page through results.
#' @param novelty Pass new or old to limit results to places
#'	the acting user hasn't been or has been, respectively.
#'	Omitting this parameter returns a mixture of old and new
#'	venues.
#' @param friendVisits Pass visited or notvisited to limit
#'	results to places the acting user's friends have or haven't
#'	been, respectively. Omitting this parameter returns a
#'	mixture of venues to which the user's friends have or
#'	haven't been.
#' @param time Pass any to retrieve results for any time of
#'	day. Omitting this parameter returns results targeted to
#'	the current time of day.
#' @param day Pass any to retrieve results for any day of the
#'	week. Omitting this parameter returns results targeted to
#'	the current day of the week.
#' @param venuePhotos Boolean flag to include a photo in the
#'	response for each venue, if one is available.  Default is 0
#'	(no photos).  Photos are returned as part of the venue JSON
#'	object.
#' @param lastVenue A venue ID to use in combination with the
#'	intent=nextVenues parameter, which returns venues users
#'	often visit after a given venue.  If intent=nextVenues is
#'	specified but lastVenue is not, the user's last check-in
#'	will be used if it is within 2 hours.  If the user has not
#'	checked in within the last 2 hours, no results will be
#'	returned.
#' @param openNow Boolean flag to only include venues that are
#'	open now. This prefers official provider hours but falls
#'	back to popular check-in hours.
#' @param sortByDistance Boolean flag to sort the results by
#'	distance instead of relevance.
#' @param price Comma separated list of price points. 
#'	Currently the valid range of price points are [1,2,3,4], 1
#'	being the least expensive, 4 being the most expensive.  For
#'	food venues, in the United States, 1 is < $10 an entree, 2
#'	is $10-$20 an entree, 3 is $20-$30 an entree, 4 is > $30 an
#'	entree.
#' @param saved Boolean flag to only include venues that the
#'	user has saved on their To-Do list or to another list.
#' @param specials Boolean flag to only include venues that
#'	have a special.
#' @param m Accepts values of "foursquare"
#' 
#' @return keywords A list of words that are suggested for the
#'	user to input as refinements. Has a count with the number
#'	of elements, and items with the actual keyword objects.
#'	Each object has a displayName as well as a keyword which is
#'	meant to be fed back into the system as a query. Presents
#'	an object with a text field that contains a warning message
#'	(i.e. not enough results, try doing X). An array of objects
#'	representing groups of recommendations. Each group contains
#'	a type such as "recommended," a human-readable (eventually
#'	localized) name such as "Recommended Places," and an array
#'	items of recommendation objects, which have an ordered list
#'	of objects which contain reasons and venue. The reasons are
#'	count and items, where each item has a type such as
#'	"social" and a message about why this place may be of
#'	interest to the acting user. The venues are compact venues
#'	that include stats and hereNow data. We encourage clients
#'	to be robust against the introduction or removal of group
#'	types by treating the groups as opaque objects to be
#'	displayed or by placing unfamiliar groups in a catchall
#'	group. If no radius was specified in the request, presents
#'	the radius that was used for the query (based upon the
#'	density of venues in the query area). A text name for the
#'	location the user searched, e.g. "SoHo". A full text name
#'	for the location the user searched, e.g. "SoHo, New York".
#'	A message to the user based on their current context, e.g.
#'	"Suggestions for Tuesday afternoon".
#' 
#' @examples
#' venues.explore(ll="44.3,37.2", near="Chicago, IL",
#'	llAcc="10000.0", alt="0", altAcc="10000.0", radius="250",
#'	section="food", query="donuts", limit="10", offset="20",
#'	novelty="new", friendVisits="visited", time="any",
#'	day="any", venuePhotos="1",
#'	lastVenue="4ef0e7cf7beb5932d5bdeb4e", openNow="1",
#'	sortByDistance="1", price="2,3", saved="1", specials="1")
venues.explore <- function(ll=NULL, near=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, radius=NULL, section=NULL, query=NULL, limit=NULL, offset=NULL, novelty=NULL, friendVisits=NULL, time=NULL, day=NULL, venuePhotos=NULL, lastVenue=NULL, openNow=NULL, sortByDistance=NULL, price=NULL, saved=NULL, specials=NULL,m="foursquare"){
  fq_GET(end_point="venues/explore",params=as.list( environment() ),requiresActingUsr="No")
}

#' Managed Venues
#' 
#'  Get a list of venues the current user manages. 
#' 
#' Get a list of venues the current user manages.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param limit Number of managed venues to return. Defaults to
#'	100, maximum is 1000.
#' @param offset Number of venues to skip over for paging.
#'	Defaults to 0.
#' @param m Accepts values of "foursquare"
#' 
#' @return venues A count and items of compact venues the user
#'	manages.
#' 
#' @examples
#' venues.managed(limit="100", offset="500")
venues.managed <- function(limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point="venues/managed",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Search Venues
#' 
#'  Returns a list of venues near the current location.
#' 
#' Returns a list of venues near the current location,
#'	optionally matching a search term.  To ensure the best
#'	possible results, pay attention to the intent parameter
#'	below. And if you're looking for "top" venues or
#'	recommended venues, use the explore endpoint instead.  If
#'	lat and long is provided, each venue includes a distance.
#'	If authenticated, the method will return venue metadata
#'	related to you and your friends. If you do not
#'	authenticate, you will not get this data.  Note that most
#'	of the fields returned inside venue can be optional. The
#'	user may create a venue that has no address, city or state
#'	(the venue is created instead at the geolat/geolong
#'	specified). Your client should handle these conditions
#'	safely.  You'll also notice a stats block that reveals some
#'	count data about the venue. herenow shows the number of
#'	people currently there (this value can be 0).  This
#'	endpoint is part of the venues API.
#' 
#' @param ll required unless near is provided. Latitude and
#'	longitude of the user's location. (Required for query
#'	searches). Optional if using intent=global
#' @param near required unless ll is provided. A string naming
#'	a place in the world. If the near string is not geocodable,
#'	returns a failed_geocode error. Otherwise, searches within
#'	the bounds of the geocode. Adds a geocode object to the
#'	response. (Required for query searches)
#' @param llAcc Accuracy of latitude and longitude, in meters.
#'	(Does not currently affect search results.)
#' @param alt Altitude of the user's location, in meters. (Does
#'	not currently affect search results.)
#' @param altAcc Accuracy of the user's altitude, in meters.
#'	(Does not currently affect search results.)
#' @param query A search term to be applied against venue
#'	names.
#' @param limit Number of results to return, up to 50.
#' @param intent One of the values below, indicating your
#'	intent in performing the search. If no value is specified,
#'	defaults to checkin.   checkin Finds results that the
#'	current user (or, for userless requests, a typical user) is
#'	likely to check in to at the provided ll at the current
#'	moment in time. This is the intent we recommend most apps
#'	use.   browse Find venues within a given area. Unlike the
#'	checkin intent, browse searches an entire region instead of
#'	only finding Venues closest to a point. You must define a
#'	region to search be including either the ll and radius
#'	parameters, or the sw and ne. The region will be a
#'	spherical cap if you include the ll and radius parameters,
#'	or it will be a bounding quadrangle if you include the sw
#'	and ne parameters.   global Finds the most globally
#'	relevant venues for the search, independent of location.
#'	Ignores all other parameters other than query and limit.  
#'	match Finds venues that are are nearly-exact matches for
#'	the given parameters. This intent is highly sensitive to
#'	the provided location. We recommend using this intent only
#'	when trying to correlate an existing place database with
#'	Foursquare's. The results will be sorted best match first,
#'	taking distance and spelling mistakes/variations into
#'	account. query and ll are the only required parameters for
#'	this intent, but matching also supports phone, address,
#'	city, state, zip, and twitter. There's no specified format
#'	for these parameters—we do our best to normalize them and
#'	drop them from the search if unsuccessful.  
#' @param radius Limit results to venues within this many
#'	meters of the specified location. Defaults to a city-wide
#'	area. Only valid for requests with intent=browse, or
#'	requests with intent=checkin and categoryId or query. Does
#'	not apply to match intent requests. The maximum supported
#'	radius is currently 100,000 meters.
#' @param sw With ne, limits results to the bounding quadrangle
#'	defined by the latitude and longitude given by sw as its
#'	south-west corner, and ne as its north-east corner. The
#'	bounding quadrangle is only supported for intent=browse
#'	searches. Not valid with ll or radius. Bounding quadrangles
#'	with an area up to approximately 10,000 square kilometers
#'	are supported.
#' @param ne See sw
#' @param categoryId A comma separated list of categories to
#'	limit results to. If you specify categoryId specifying a
#'	radius may improve results. If specifying a top-level
#'	category, all sub-categories will also match the query.
#'	Does not apply to match intent requests.
#' @param url A third-party URL which we will attempt to match
#'	against our map of venues to URLs.
#' @param providerId Identifier for a known third party that is
#'	part of our map of venues to URLs, used in conjunction with
#'	linkedId.
#' @param linkedId 1002207971611 Identifier used by third party
#'	specified in providerId, which we will attempt to match
#'	against our map of venues to URLs.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return venues An array of compact venues.
#' 
#' @examples
#' venues.search(ll="44.3,37.2", near="Chicago, IL",
#'	llAcc="10000.0", alt="0", altAcc="10000.0", query="donuts",
#'	limit="10", intent="checkin", radius="800", sw="44.3,37.2",
#'	ne="44.1,37.4", categoryId="asad13242l,btbe24353m",
#'	url="http://nymag.com/listings/restaurant/abistro/",
#'	providerId="nymag", linkedId="4247")
venues.search <- function(ll=NULL, near=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, query=NULL, limit=NULL, intent=NULL, radius=NULL, sw=NULL, ne=NULL, categoryId=NULL, url=NULL, providerId=NULL, linkedId=NULL,m="foursquare"){
  fq_GET(end_point="venues/search",params=as.list( environment() ),requiresActingUsr="No")
}

#' Suggest Completion Venues
#' 
#'  Returns a list of mini-venues.
#' 
#' Returns a list of mini-venues partially matching the search
#'	term, near the location.
#' 
#' @param ll required Latitude and longitude of the user's
#'	location. (Required for query searches)
#' @param near required unless ll is provided. A string naming
#'	a place in the world. If the near string is not geocodable,
#'	returns a failed_geocode error. Otherwise, searches within
#'	the bounds of the geocode. Adds a geocode object to the
#'	response. (Required for query searches)
#' @param llAcc Accuracy of latitude and longitude, in meters.
#'	(Does not currently affect search results.)
#' @param alt Altitude of the user's location, in meters. (Does
#'	not currently affect search results.)
#' @param altAcc Accuracy of the user's altitude, in meters.
#'	(Does not currently affect search results.)
#' @param query required A search term to be applied against
#'	titles. Must be at least 3 characters long.
#' @param limit Number of results to return, up to 100.
#' @param radius Limit results to venues within this many
#'	meters of the specified location. Defaults to a city-wide
#'	area. The maximum supported radius is currently
#'	80,000 meters.
#' @param sw With ne, limits results to the bounding quadrangle
#'	defined by the latitude and longitude given by sw as its
#'	south-west corner, and ne as its north-east corner. The
#'	bounding quadrangle is only supported for intent=browse
#'	searches. Not valid with ll or radius. Bounding quadrangles
#'	with an area up to approximately 10,000 square kilometers
#'	are supported.
#' @param ne See sw
#' @param m Accepts values of "foursquare"
#' 
#' @return minivenues An array of venues that only contain id,
#'	name, location, and categories.
#' 
#' @examples
#' venues.suggestcompletion(ll="44.3,37.2", near="Chicago, IL",
#'	llAcc="10000.0", alt="0", altAcc="10000.0",
#'	query="foursqu", limit="30", radius="800", sw="44.3,37.2",
#'	ne="44.1,37.4")
venues.suggestcompletion <- function(ll=NULL, near=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, query=NULL, limit=NULL, radius=NULL, sw=NULL, ne=NULL,m="foursquare"){
  fq_GET(end_point="venues/suggestcompletion",params=as.list( environment() ),requiresActingUsr="No")
}

#' Venue Time Series Data
#' 
#'  Get daily venue stats for a list of venues.
#' 
#' Get daily venue stats for a list of venues over a time
#'	range.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param venueId A comma-separated list of venue ids to
#'	retrieve series data for. The current user must be the
#'	manager of all venues specified.
#' @param startAt required. The start of the time range to
#'	retrieve stats for (seconds since epoch).
#' @param endAt The end of the time range to retrieve stats for
#'	(seconds since epoch). If omitted, the current time is
#'	assumed.
#' @param fields Specifies which fields to return. May be one
#'	or more of totalCheckins, newCheckins, uniqueVisitors,
#'	sharing, genders, ages, hours, separated by commas.
#' @param m Accepts values of "foursquare"
#' 
#' @return timeseries An array of venue time series data
#'	objects, one for each venue.
#' 
#' @examples
#' venues.timeseries(venueId="AVNU234,AVNU567",
#'	startAt="1284286794", endAt="1284286794",
#'	fields="totalCheckins,newCheckins")
venues.timeseries <- function(venueId=NULL, startAt=NULL, endAt=NULL, fields=NULL,m="foursquare"){
  fq_GET(end_point="venues/timeseries",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Trending Venues
#' 
#'  Returns a list of venues near the current location.
#' 
#' Returns a list of venues near the current location with the
#'	most people currently checked in.  This endpoint is part of
#'	the venues API.
#' 
#' @param ll required Latitude and longitude of the user's
#'	location.
#' @param limit Number of results to return, up to 50.
#' @param radius Radius in meters, up to approximately 2000
#'	meters.
#' @param m Accepts values of "swarm"
#' 
#' @return venues An array of venues that are currently
#'	trending, with their hereNow populated.
#' 
#' @examples
#' venues.trending(ll="44.3,37.2", limit="10", radius="100")
venues.trending <- function(ll=NULL, limit=NULL, radius=NULL,m="swarm"){
  fq_GET(end_point="venues/trending",params=as.list( environment() ),requiresActingUsr="No")
}

#' Venue Events
#' 
#'  Allows you to access information about the current events.
#' 
#' Allows you to access information about the current events at
#'	a place.  At this time we are only able to distribute Music
#'	events and limited information about Movie events via this
#'	endpoint due to partner restrictions.
#' 
#' @param VENUE_ID required The venue id for which events are
#'	being requested.
#' @param m Accepts values of "foursquare"
#' 
#' @return events A count and items of event items. Also
#'	includes a "summary" string describing the set of events at
#'	the venue.
#' 
#' @examples
#' venues.events(VENUE_ID="AVNU234")
venues.events <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/events"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Venue Here Now
#' 
#'  Provides a count of how many people are at a given venue.  
#' 
#' Provides a count of how many people are at a given venue. 
#'	Returns a list of  friends and friends-of-friends at the
#'	venue for authenticated requests. If the acting user is
#'	currently checked in to the venue, then a list of other
#'	users is also returned.  Authenticated requests by the
#'	manager of a venue will always return the list of users
#'	“here now” for that venue  Do not aggregate information
#'	from this endpoint across multiple venues.  This endpoint
#'	is part of the venues API.
#' 	Note: This function requires acting user
#' 
#' @param VENUE_ID required ID of venue to retrieve
#' @param limit Number of results to return, up to 500.
#' @param offset Used to page through results.
#' @param m Accepts values of "swarm"
#' 
#' @return hereNow A count and items where items are checkins.
#' 
#' @examples
#' venues.herenow(VENUE_ID="A9ABCD", limit="100", offset="100")
venues.herenow <- function(VENUE_ID=NULL, limit=NULL, offset=NULL,m="swarm"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/herenow"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Venue Hours
#' 
#'  Returns hours for a venue.  
#' 
#' Returns hours for a venue.
#' 
#' @param VENUE_ID required The venue id for which hours are
#'	being requested.
#' @param m Accepts values of "foursquare"
#' 
#' @return hours An array of timeframes of open hours. An array
#'	of timeframes of popular hours.
#' 
#' @examples
#' venues.hours(VENUE_ID="XXX123YYYY")
venues.hours <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/hours"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Users who have liked a venue
#' 
#'  Returns friends and a total count of users who have liked.
#' 
#' Returns friends and a total count of users who have liked
#'	this venue.
#' 
#' @param VENUE_ID The ID of the venue to get likes for.
#' @param m Accepts values of "foursquare"
#' 
#' @return likes A count and groups of users who like this
#'	venue. Groups generally include friends and others
#'	(indicating the relationship to the acting user), but are
#'	subject to change.
#' 
#' @examples
#' venues.likes(VENUE_ID="HZXXY3Y")
venues.likes <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/likes"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Links For a Venue
#' 
#'  Returns URLs or identifiers from third parties.
#' 
#' Returns URLs or identifiers from third parties that have
#'	been applied to this venue, such as how the New York Times
#'	refers to this venue and a URL for additional information
#'	from nytimes.com. This is part of the foursquare Venue Map.
#' 
#' @param VENUE_ID required The venue you want annotations for.
#' @param m Accepts values of "foursquare"
#' 
#' @return links A count and items of links.
#' 
#' @examples
#' venues.links(VENUE_ID="XXX123YYYY")
venues.links <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/links"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Lists
#' 
#'  The lists that this venue appears on.
#' 
#' The lists that this venue appears on
#' 
#' @param VENUE_ID Identity of a venue to get lists for.
#' @param group can be created, edited, followed, friends,
#'	other. If no acting user is present, only other is
#'	supported.
#' @param limit Number of results to return, up to 200.
#' @param offset Used to page through results. Must specify a
#'	group
#' @param m Accepts values of "foursquare"
#' 
#' @return lists If group is specified, it will contain count
#'	and items of lists. If no group is specified, it will
#'	contain a groups array containing elements, each with type,
#'	name, an optional count, and an optional items.  The type
#'	field can be passed in as a group to get more elements. If
#'	present count represents the total number of elements.  If
#'	not specified, there are a potentially unbounded number of
#'	elements. items contains a sample of the lists.
#' 
#' @examples
#' venues.listed(VENUE_ID="HZXXY3Y", group="edited",
#'	limit="30", offset="300")
venues.listed <- function(VENUE_ID=NULL, group=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/listed"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Venue Menu
#' 
#'  Returns menu information for a venue.
#' 
#' Returns menu information for a venue.   In some cases, menu
#'	information is provided by our partners. When displaying
#'	the information from a partner, you must attribute them
#'	using the attribution information included in the provider
#'	field. Not all menu information available on Foursquare is
#'	able to be redistributed through our API.
#' 
#' @param VENUE_ID required The venue id for which menu is
#'	being requested.
#' @param m Accepts values of "foursquare"
#' 
#' @return provider The name and attribution information for
#'	the menu provider. If attribution information is provided,
#'	you must display both the attributionImage and
#'	attributionText as well as link to attributionLink. A count
#'	and items of menu.
#' 
#' @examples
#' venues.menu(VENUE_ID="AVNU234")
venues.menu <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/menu"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Next Venues
#' 
#'  Returns other venues that people often check.
#' 
#' Returns venues that people often check in to after the
#'	current venue. Up to 5 venues are returned in each query,
#'	and results are sorted by how many people have visited that
#'	venue after the current one. Homes are never returned in
#'	results.
#' 
#' @param VENUE_ID required ID of the venue you want to see
#'	next venue information about
#' @param m Accepts values of "swarm"
#' 
#' @return nextVenues A count and items where items are compact
#'	venues.
#' 
#' @examples
#' venues.nextvenues(VENUE_ID="A9ABCD")
venues.nextvenues <- function(VENUE_ID=NULL,m="swarm"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/nextvenues"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Photos from a Venue
#' 
#'  Returns photos for a venue.  
#' 
#' Returns photos for a venue.
#' 
#' @param VENUE_ID required The venue you want photos for.
#' @param group If not specified, public venue photos are
#'	returned ordered by relevance. Pass venue for public venue
#'	photos, ordered by recency. Pass checkin for venue photos
#'	from friends (including non-public photos from recent
#'	checkins), ordered by recency. See our documentation on
#'	photos for information on how to handle the response and
#'	construct actual photo URLs.
#' @param limit Number of results to return, up to 200.
#' @param offset Used to page through results.
#' @param m Accepts values of "foursquare"
#' 
#' @return photos A count and items of photo.
#' 
#' @examples
#' venues.photos(VENUE_ID="XXX123YYYY", group="checkin",
#'	limit="100", offset="100")
venues.photos <- function(VENUE_ID=NULL, group=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/photos"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Similar Venues
#' 
#'  Returns a list of venues similar to the specified venue.  
#' 
#' Returns a list of venues similar to the specified venue.
#' 	Note: This function requires acting user
#' 
#' @param VENUE_ID required The venue you want similar venues
#'	for.
#' @param m Accepts values of "foursquare"
#' 
#' @return similarVenues A count and items of similar venues.
#' 
#' @examples
#' venues.similar(VENUE_ID="XXX123YYYY")
venues.similar <- function(VENUE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/similar"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Venue Stats
#' 
#'  Get venue stats over a given time range.
#' 
#' Get venue stats over a given time range. Only available to
#'	the manager of a venue.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param VENUE_ID required The venue id to retrieve stats for.
#' @param startAt The start of the time range to retrieve stats
#'	for (seconds since epoch). If omitted, all-time stats will
#'	be returned.
#' @param endAt The end of the time range to retrieve stats for
#'	(seconds since epoch). If omitted, the current time is
#'	assumed.
#' @param m Accepts values of "foursquare"
#' 
#' @return stats A venue stats object.
#' 
#' @examples
#' venues.stats(VENUE_ID="AVNU234", startAt="1284286794",
#'	endAt="1284286794")
venues.stats <- function(VENUE_ID=NULL, startAt=NULL, endAt=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/stats"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Tips from a Venue
#' 
#'  Returns tips for a venue.  
#' 
#' Returns tips for a venue.
#' 
#' @param VENUE_ID required The venue you want tips for.
#' @param sort One of friends, recent, or popular.
#' @param limit Number of results to return, up to 500.
#' @param offset Used to page through results.
#' @param m Accepts values of "foursquare"
#' 
#' @return tips A count and items of tips.
#' 
#' @examples
#' venues.tips(VENUE_ID="XXX123YYYY", sort="recent",
#'	limit="100", offset="100")
venues.tips <- function(VENUE_ID=NULL, sort=NULL, limit=NULL, offset=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venues/",VENUE_ID,"/tips"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Claim a Venue
#' 
#'  Claim a venue for the user.
#' 
#' Claim a venue for the user. If your OAuth Consumer is set up
#'	with the proper privileges, you can use this endpoint to
#'	claim a venue on behalf of a user without the claim having
#'	to be approved by foursquare staff.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param visible (optional, default true) whether role as
#'	manager is visible on the venue page
#' @param m Accepts values of "foursquare"
#' 
#' @return Empty response 
#' 
#' @examples
#' venues.claim(visible="false")
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

#' Like or unlike a venue
#' 
#'  Allows the acting user to like or unlike a venue. 
#' 
#' Allows the acting user to like or unlike a venue.
#' 	Note: This function requires acting user
#' 
#' @param VENUE_ID required The venue to like or unlike.
#' @param set If 1, like this venue. If 0 unlike (un-do a
#'	previous like) it. Default value is 1.
#' @param m Accepts values of "foursquare"
#' 
#' @return likes Updated count and groups of users who like
#'	this venue. Groups generally include friends and others
#'	(indicating the relationship to the acting user), but are
#'	subject to change.
#' 
#' @examples
#' venues.like(VENUE_ID="XXX123YYYY", set="1")
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
#' Venue Group Details
#' 
#'  Get venue group details. 
#' 
#' Get venue group details.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param GROUP_ID The ID of the venue group to retrieve
#'	additional information for.
#' @param m Accepts values of "foursquare"
#' 
#' @return venuegroup A venue group object.
#' 
#' @examples
#' venuegroups.venuegroups(GROUP_ID="IHR8THISVNU")
venuegroups.venuegroups <- function(GROUP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venuegroups/",GROUP_ID,"/venuegroups"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Create a new venue group
#' 
#'  Create a venue group.
#' 
#' Create a venue group. If the venueId parameter is specified,
#'	then the endpoint will add the specified venues to the
#'	venue group. If it is not possible to add all of the
#'	specified venues to the group, then creation of the venue
#'	group will fail entirely.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param name Required. The name to give the group.
#' @param venueId Comma-delimited list of venue IDs to add to
#'	the group. If this parameter is not specified, then the
#'	venue group will initially be empty.
#' @param m Accepts values of "foursquare"
#' 
#' @return venue group A venue group object.
#' 
#' @examples
#' venuegroups.add(name="Think Coffees", venueId="IHR8THISVNU")
venuegroups.add <- function(name=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point="venuegroups/add",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Delete a Venue Group
#' 
#'  Delete a venue group. 
#' 
#' Delete a venue group.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param GROUP_ID required The ID of the venue group to
#'	delete.
#' @param m Accepts values of "foursquare"
#' 
#' @return Success code or error message 
#' 
#' @examples
#' venuegroups.delete(GROUP_ID="IHR8THISVNU")
venuegroups.delete <- function(GROUP_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/delete"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.list <- function(m="foursquare"){
  fq_GET(end_point="venuegroups/list",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Venue Group Time Series Data
#' 
#'  Get daily venue stats for the venues in a group.
#' 
#' Get daily venue stats for the venues in a group over a time
#'	range.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param GROUP_ID The venue group to retrieve series data for.
#' @param startAt required. The start of the time range to
#'	retrieve stats for (seconds since epoch).
#' @param endAt The end of the time range to retrieve stats for
#'	(seconds since epoch). If omitted, the current time is
#'	assumed.
#' @param fields Specifies which fields to return. May be one
#'	or more of totalCheckins, newCheckins, uniqueVisitors,
#'	sharing, genders, ages, hours, separated by commas.
#' @param m Accepts values of "foursquare"
#' 
#' @return timeseries An array of venue time series data
#'	objects, one for each venue in the group.
#' 
#' @examples
#' venuegroups.timeseries(GROUP_ID="AGRP234",
#'	startAt="1284286794", endAt="1284286794",
#'	fields="totalCheckins,newCheckins")
venuegroups.timeseries <- function(GROUP_ID=NULL, startAt=NULL, endAt=NULL, fields=NULL,m="foursquare"){
  fq_GET(end_point=paste0("venuegroups/",GROUP_ID,"/timeseries"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Add Venue
#' 
#'  Add a venue to a venue group. 
#' 
#' Add a venue to a venue group.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param GROUP_ID required The ID of the venue group to modify
#' @param venueId required comma-delimited list of venue IDs to
#'	add to the group
#' @param m Accepts values of "foursquare"
#' 
#' @return Success code or error message 
#' 
#' @examples
#' venuegroups.addvenue(GROUP_ID="IHR8THISVNU",
#'	venueId="IHR8THISVNU")
venuegroups.addvenue <- function(GROUP_ID=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/addvenue"),params=as.list( environment() ),requiresActingUsr="Yes")
}
venuegroups.edit <- function(VENUEGROUP_ID=NULL, name=NULL, city=NULL, state=NULL, zip=NULL, phone=NULL, categoryId=NULL, twitter=NULL, description=NULL, url=NULL, hours=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",VENUEGROUP_ID,"/edit"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Remove Venue
#' 
#'  Remove a venue from a venue group. 
#' 
#' Remove a venue from a venue group.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param GROUP_ID required The ID of the venue group to modify
#' @param venueId required comma-delimited list of venue IDs to
#'	remove from the group
#' @param m Accepts values of "foursquare"
#' 
#' @return Success code or error message 
#' 
#' @examples
#' venuegroups.removevenue(GROUP_ID="IHR8THISVNU",
#'	venueId="IHR8THISVNU")
venuegroups.removevenue <- function(GROUP_ID=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/removevenue"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Update Venue Group
#' 
#'  Updates a venue group.
#' 
#' Updates a venue group. At least one of the name and venueId
#'	parameters must be specified.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param GROUP_ID required The ID of the venue group to modify
#' @param name If specified, the new name to give to the group.
#' @param venueId If specified, a comma-delimited list of venue
#'	IDs that will become the new set of venue IDs for the
#'	group.
#' @param m Accepts values of "foursquare"
#' 
#' @return venue group A venue group object.
#' 
#' @examples
#' venuegroups.update(GROUP_ID="IHR8THISVNU", name="Think
#'	Coffees", venueId="IHR8THISVNU")
venuegroups.update <- function(GROUP_ID=NULL, name=NULL, venueId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("venuegroups/",GROUP_ID,"/update"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Check-in Details
#' 
#'  Get details of a checkin. 
#' 
#' Get details of a checkin.
#' 	Note: This function requires acting user
#' 
#' @param CHECKIN_ID The ID of the checkin to retrieve
#'	additional information for.
#' @param signature When checkins are sent to public feeds such
#'	as Twitter, foursquare appends a signature (s=XXXXXX)
#'	allowing users to bypass the friends-only access check on
#'	checkins. The same value can be used here for programmatic
#'	access to otherwise inaccessible checkins. Callers should
#'	use the bit.ly API to first expand 4sq.com links.
#' @param m Accepts values of "swarm"
#' 
#' @return checkin A complete checkin object.
#' 
#' @examples
#' checkins.checkins(CHECKIN_ID="IHR8THISVNU",
#'	signature="ASDJKASLJDLA")
checkins.checkins <- function(CHECKIN_ID=NULL, signature=NULL,m="swarm"){
  fq_GET(end_point=paste0("checkins/",CHECKIN_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Create a check-in
#' 
#'  Allows you to check in to a place. 
#' 
#' Allows you to check in to a place.
#' 	Note: This function requires acting user
#' 
#' @param venueId required The venue where the user is checking
#'	in. Find venue IDs by searching or from historical APIs.
#' @param eventId The event the user is checking in to.
#' @param shout A message about your check-in. The maximum
#'	length of this field is 140 characters.
#' @param mentions Mentions in your check-in. This parameter is
#'	a semicolon-delimited list of mentions. A single mention is
#'	of the form "start,end,userid", where start is the index of
#'	the first character in the shout representing the mention,
#'	end is the index of the first character in the shout after
#'	the mention, and userid is the userid of the user being
#'	mentioned.  If userid is prefixed with "fbu-", this
#'	indicates a Facebook userid that is being mention. 
#'	Character indices in shouts are 0-based.
#' @param broadcast Who to broadcast this check-in to. Accepts
#'	a comma-delimited list of values:  private (off the grid)
#'	or public (share with friends) facebook share on facebook
#'	twitter share on twitter followers share with followers
#'	(celebrity mode users only)   If no valid value is found,
#'	the default is public.
#' @param ll Latitude and longitude of the user's location.
#'	Only specify this field if you have a GPS or other device
#'	reported location for the user at the time of check-in.
#' @param llAcc Accuracy of the user's latitude and longitude,
#'	in meters.
#' @param alt Altitude of the user's location, in meters.
#' @param altAcc Vertical accuracy of the user's location, in
#'	meters.
#' @param m Accepts values of "swarm"
#' 
#' @return checkin A checkin object. A post-checkin
#'	notifications object.
#' 
#' @examples
#' checkins.add(venueId="IHR8THISVNU", eventId="UHR8THISVNT",
#'	shout="There are crayons! Crayons!",
#'	mentions="5,10,HZXXY3Y;15,20,GZYYZ3Z;25,30,fbu-GZXY13Y",
#'	broadcast="public,twitter", ll="33.7,44.2", llAcc="1",
#'	alt="0", altAcc="1")
checkins.add <- function(venueId=NULL, eventId=NULL, shout=NULL, mentions=NULL, broadcast=NULL, ll=NULL, llAcc=NULL, alt=NULL, altAcc=NULL,m="swarm"){
  fq_POST(end_point="checkins/add",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Recent checkins by friends
#' 
#'  Returns a list of recent checkins from friends. 
#' 
#' Returns a list of recent checkins from friends.
#' 	Note: This function requires acting user
#' 
#' @param ll Latitude and longitude of the user's location, so
#'	response can include distance.
#' @param limit Number of results to return, up to 100.
#' @param afterTimestamp Seconds after which to look for
#'	checkins, e.g. for looking for new checkins since the last
#'	fetch. If more than limit results are new since then, this
#'	is ignored. Checkins created prior to this timestamp will
#'	still be returned if they have new comments or photos,
#'	making it easier to poll for all new activity.
#' @param m Accepts values of "swarm"
#' 
#' @return checkins An array of checkin objects with user
#'	details present. Usually, venue details are also present,
#'	depending on whether the checkin has a venue.
#' 
#' @examples
#' checkins.recent(ll="44.3,37.2", limit="20",
#'	afterTimestamp="123456")
checkins.recent <- function(ll=NULL, limit=NULL, afterTimestamp=NULL,m="swarm"){
  fq_GET(end_point="checkins/recent",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Users who have liked a checkin
#' 
#'  Returns friends and a total count of users who have liked.
#' 
#' Returns friends and a total count of users who have liked
#'	this checkin.
#' 
#' @param CHECKIN_ID The ID of the checkin to get likes for.
#' @param m Accepts values of "swarm"
#' 
#' @return likes A count and groups of users who like this
#'	checkin. Groups generally include friends and others
#'	(indicating the relationship to the acting user), but are
#'	subject to change.
#' 
#' @examples
#' checkins.likes(CHECKIN_ID="HZXXY3Y")
checkins.likes <- function(CHECKIN_ID=NULL,m="swarm"){
  fq_GET(end_point=paste0("checkins/",CHECKIN_ID,"/likes"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Add a comment to a check-in
#' 
#'  Comment on a checkin-in.
#' 
#' Comment on a checkin-in
#' 	Note: This function requires acting user
#' 
#' @param CHECKIN_ID The ID of the checkin to add a comment to.
#' @param text The text of the comment, up to 200 characters.
#' @param mentions Mentions in your check-in. This parameter is
#'	a semicolon-delimited list of mentions. A single mention is
#'	of the form "start,end,userid", where start is the index of
#'	the first character in the shout representing the mention,
#'	end is the index of the first character in the shout after
#'	the mention, and userid is the userid of the user being
#'	mentioned. Character indices in shouts are 0-based.
#' @param m Accepts values of "swarm"
#' 
#' @return comment The newly-created comment.
#' 
#' @examples
#' checkins.addcomment(CHECKIN_ID="IHR8THISVNU",
#'	text="Awesome!", mentions="5,10,HZXXY3Y;15,20,GZYYZ3Z")
checkins.addcomment <- function(CHECKIN_ID=NULL, text=NULL, mentions=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/addcomment"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Add a Post
#' 
#'  Post user generated content from an external app.
#' 
#' Post user generated content from an external app to a
#'	check-in. This post will be accessible to anyone who can
#'	view the details of the check-in.  Only call this endpoint
#'	if the user has chosen to post some information from the
#'	application. If you want to create a post containing a
#'	photo, use photos/add instead.
#' 	Note: This function requires acting user
#' 
#' @param CHECKIN_ID The ID of the checkin to add a post to.
#' @param text The text of the post, up to 200 characters.
#' @param url Link for more details. This page will be opened
#'	in an embedded web view in the foursquare application,
#'	unless contentId is specified and a native link handler is
#'	registered and present. We support the following URL
#'	schemes: http, https, foursquare, mailto, tel, and sms.
#' @param contentId Identifier for the post to be used in a
#'	native link, up to 50 characters. A url must also be
#'	specified in the request.
#' @param m Accepts values of "swarm"
#' 
#' @return post The newly-created post.
#' 
#' @examples
#' checkins.addpost(CHECKIN_ID="IHR8THISVNU", text="Awesome!",
#'	url="http://your.site.com/path", contentId="3ER4GN93AFWE")
checkins.addpost <- function(CHECKIN_ID=NULL, text=NULL, url=NULL, contentId=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/addpost"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Remove commment from check-in
#' 
#'  Remove a comment from a checkin.
#' 
#' Remove a comment from a checkin, if the acting user is the
#'	author or the owner of the checkin.
#' 	Note: This function requires acting user
#' 
#' @param CHECKIN_ID The ID of the checkin to remove a comment
#'	from.
#' @param commentId The id of the comment to remove.
#' @param m Accepts values of "swarm"
#' 
#' @return checkin The checkin, minus this comment.
#' 
#' @examples
#' checkins.deletecomment(CHECKIN_ID="IHR8THISVNU",
#'	commentId="ABZQ200")
checkins.deletecomment <- function(CHECKIN_ID=NULL, commentId=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/deletecomment"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Like or unlike a checkin
#' 
#'  Allows the acting user to like or unlike a checkin. 
#' 
#' Allows the acting user to like or unlike a checkin.
#' 	Note: This function requires acting user
#' 
#' @param CHECKIN_ID required The checkin to like or unlike.
#' @param set If 1, like this checkin. If 0 unlike (un-do a
#'	previous like) it. Default value is 1.
#' @param m Accepts values of "swarm"
#' 
#' @return likes Updated count and groups of users who like
#'	this checkin. Groups generally include friends and others
#'	(indicating the relationship to the acting user), but are
#'	subject to change.
#' 
#' @examples
#' checkins.like(CHECKIN_ID="XXX123YYYY", set="1")
checkins.like <- function(CHECKIN_ID=NULL, set=NULL,m="swarm"){
  fq_POST(end_point=paste0("checkins/",CHECKIN_ID,"/like"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Tip detail
#' 
#'  Gives details about a tip.
#' 
#' Gives details about a tip, including which users (especially
#'	friends) have marked the tip to-do.
#' 
#' @param TIP_ID required ID of tip to retrieve
#' @param m Accepts values of "foursquare"
#' 
#' @return tip A complete tip.
#' 
#' @examples
#' tips.tips(TIP_ID="A9ABCD")
tips.tips <- function(TIP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID),params=as.list( environment() ),requiresActingUsr="No")
}

#' Add a tip
#' 
#'  Allows you to add a new tip at a venue. 
#' 
#' Allows you to add a new tip at a venue.
#' 	Note: This function requires acting user
#' 
#' @param venueId required The venue where you want to add this
#'	tip.
#' @param text required The text of the tip, up to 200
#'	characters.
#' @param url A URL related to this tip.
#' @param broadcast Whether to broadcast this tip. Send twitter
#'	if you want to send to twitter, facebook if you want to
#'	send to facebook, or twitter,facebook if you want to send
#'	to both.
#' @param m Accepts values of "foursquare"
#' 
#' @return tip The newly-added tip.
#' 
#' @examples
#' tips.add(venueId="XXX123YYYY", text="The donuts are tasty!",
#'	url="http://blog.zagat.com/fast-food-survey-results-are-her
#'	e", broadcast="twitter")
tips.add <- function(venueId=NULL, text=NULL, url=NULL, broadcast=NULL,m="foursquare"){
  fq_POST(end_point="tips/add",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Users who have liked a tip
#' 
#'  Returns friends and a total count of users who have liked this tip
#' 
#' Returns friends and a total count of users who have liked
#'	this tip.
#' 
#' @param TIP_ID The ID of the tip to get likes for.
#' @param m Accepts values of "foursquare"
#' 
#' @return likes A count and groups of users who like this tip.
#'	Groups generally include friends and others (indicating the
#'	relationship to the acting user), but are subject to
#'	change.
#' 
#' @examples
#' tips.likes(TIP_ID="HZXXY3Y")
tips.likes <- function(TIP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID,"/likes"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Lists
#' 
#'  The lists that this tip appears on. 
#' 
#' The lists that this tip appears on
#' 
#' @param TIP_ID Identity of a tip to get lists for.
#' @param group can be created, edited, followed, friends,
#'	other. If no acting user is present, only other is
#'	supported.
#' @param m Accepts values of "foursquare"
#' 
#' @return lists If group is specified, it will contain count
#'	and items of lists. If no group is specified, it will
#'	contain a groups array containing elements, each with type,
#'	name, an optional count, and an optional items.  The type
#'	field can be passed in as a group to get more elements. If
#'	present count represents the total number of elements.  If
#'	not specified, there are a potentially unbounded number of
#'	elements. items contains a sample of the lists.
#' 
#' @examples
#' tips.listed(TIP_ID="HZXXY3Y", group="edited")
tips.listed <- function(TIP_ID=NULL, group=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID,"/listed"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Users who have saved a tip
#' 
#'  Returns friends and a total count of users who have saved this tip.
#' 
#' Returns friends and a total count of users who have saved
#'	this tip.
#' 
#' @param TIP_ID The ID of the tip to get saves for.
#' @param m Accepts values of "foursquare"
#' 
#' @return saves A count and groups of users who saved this
#'	tip. Groups generally include friends and others
#'	(indicating the relationship to the acting user), but are
#'	subject to change.
#' 
#' @examples
#' tips.saves(TIP_ID="HZXXY3Y")
tips.saves <- function(TIP_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("tips/",TIP_ID,"/saves"),params=as.list( environment() ),requiresActingUsr="No")
}
tips.flag <- function(TIP_ID=NULL, comment=NULL, problem=NULL,m="foursquare"){
  fq_POST(end_point=paste0("tips/",TIP_ID,"/flag"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Like or unlike a tip
#' 
#'  Allows the acting user to like or unlike a tip. 
#' 
#' Allows the acting user to like or unlike a tip.
#' 	Note: This function requires acting user
#' 
#' @param TIP_ID required The tip to like or unlike.
#' @param set If 1, like this tip. If 0 unlike (un-do a
#'	previous like) it. Default value is 1.
#' @param m Accepts values of "foursquare"
#' 
#' @return likes Updated count and groups of users who like
#'	this tip. Groups generally include friends and others
#'	(indicating the relationship to the acting user), but are
#'	subject to change.
#' 
#' @examples
#' tips.like(TIP_ID="XXX123YYYY", set="1")
tips.like <- function(TIP_ID=NULL, set=NULL,m="foursquare"){
  fq_POST(end_point=paste0("tips/",TIP_ID,"/like"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Unmark a tip as to-do
#' 
#'  Allows you to remove a tip from your to-do list.
#' 
#' Allows you to remove a tip from your to-do list.  Note: The
#'	lists deleteitem endpoint is now the preferred way to
#'	remove a tip from a user's To-do list
#'	(/v2/lists/USER_ID/todos/deleteitem).
#' 	Note: This function requires acting user
#' 
#' @param TIP_ID required The tip you want to unmark.
#' @param m Accepts values of "foursquare"
#' 
#' @return tip The tip being acted on.
#' 
#' @examples
#' tips.unmark(TIP_ID="XXX123YYYY")
tips.unmark <- function(TIP_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("tips/",TIP_ID,"/unmark"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' List Detail
#' 
#'  Gives details about a list.
#' 
#' Gives details about a list.  The list id parameter can be a
#'	user-created list id as well as one of either USER_ID/tips
#'	or USER_ID/todos. Note that a user's todos are only visible
#'	to their friends.
#' 
#' @param LIST_ID required id for a user-created (e.g.
#'	/v2/lists/12381902N) or followed list or one of either
#'	USER_ID/tips (e.g. /v2/lists/32/tips) or USER_ID/todos.
#' @param limit Number of results to return, up to 200.
#' @param offset The number of results to skip. Used to page
#'	through results.
#' @param llBounds optional Restricts the returned results to
#'	the input bounding box.
#' @param categoryId optional Restricts the returned results to
#'	venues matching the input category id.
#' @param sort optional Sorts the list items. Possible values
#'	are recent and nearby. recent sorts the list items by the
#'	date added to the list. nearby sorts the list items by the
#'	distance from the center of the provided llBounds.
#' @param m Accepts values of "foursquare"
#' 
#' @return list A list object.
#' 
#' @examples
#' lists.lists(LIST_ID="A9ABCD", limit="100", offset="100",
#'	llBounds="30.13800,-98.16009,30.40485,-97.28118",
#'	categoryId="4bf58dd8d48988d10e941735", sort="recent")
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

#' Add a List
#' 
#'  Allows users to create a new list. 
#' 
#' Allows users to create a new list.
#' 	Note: This function requires acting user
#' 
#' @param name required The name of the list.
#' @param description The description of the list.
#' @param collaborative Boolean indicating if this list can be
#'	edited by friends.
#' @param photoId The id of a photo that should be set as the
#'	list photo.
#' @param m Accepts values of "foursquare"
#' 
#' @return list The list that was just created
#' 
#' @examples
#' lists.add(name="Max's Favorite Ramen Spots",
#'	description="Must visit ramen spots according to Max.",
#'	collaborative="true", photoId="IHR8THISPHT")
lists.add <- function(name=NULL, description=NULL, collaborative=NULL, photoId=NULL,m="foursquare"){
  fq_POST(end_point="lists/add",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' List Followers
#' 
#'  Returns a count and items of users following this list.
#' 
#' Returns a count and items of users following this list. 
#'	Note: Only valid on user-created lists
#' 
#' @param LIST_ID required id for a user-created list
#' @param m Accepts values of "foursquare"
#' 
#' @return followers Returns a pageable list of compact user.
#' 
#' @examples
#' lists.followers(LIST_ID="HZXXY3Y")
lists.followers <- function(LIST_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/followers"),params=as.list( environment() ),requiresActingUsr="No")
}

#' List Item Detail
#' 
#'  Gives details about a list item.
#' 
#' Gives details about a list item
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created or followed
#'	list.
#' @param ITEM_ID required id for an item in the parent list.
#' @param m Accepts values of "foursquare"
#' 
#' @return list An item object.
#' 
#' @examples
#' lists.items(LIST_ID="A9ABCD", ITEM_ID="U7EU8R")
lists.items <- function(LIST_ID=NULL, ITEM_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID, "/", ITEM_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Users who have saved a list
#' 
#'  Friends and a total count of users who have saved this list.
#' 
#' Returns friends and a total count of users who have saved
#'	this list.
#' 
#' @param LIST_ID The ID of the list to get saves for.
#' @param m Accepts values of "foursquare"
#' 
#' @return saves A count and groups of users who saved this
#'	list. Groups generally include friends and others
#'	(indicating the relationship to the acting user), but are
#'	subject to change.
#' 
#' @examples
#' lists.saves(LIST_ID="HZXXY3Y")
lists.saves <- function(LIST_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/saves"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Suggest Photo
#' 
#'  Suggests photos that may be appropriate for this item.
#' 
#' Suggests photos that may be appropriate for this item. 
#'	Note: Only valid on user-created lists
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created list.
#' @param itemId required id of item on this list.
#' @param m Accepts values of "foursquare"
#' 
#' @return photos Returns groups user and others containing
#'	lists (a count and items of photos) of photos uploaded by
#'	this user and uploaded by other users.
#' 
#' @examples
#' lists.suggestphoto(LIST_ID="HZXXY3Y", itemId="HZXXY3Y")
lists.suggestphoto <- function(LIST_ID=NULL, itemId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/suggestphoto"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Suggest Tip
#' 
#'  Suggests tips that may be appropriate for this item.
#' 
#' Suggests tips that may be appropriate for this item.  Note:
#'	Only valid on user-created lists
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created list
#' @param itemId required id of item
#' @param m Accepts values of "foursquare"
#' 
#' @return tips Returns groups user and others containing lists
#'	(a count and items of tips) of tips created by this user
#'	and created by other users.
#' 
#' @examples
#' lists.suggesttip(LIST_ID="HZXXY3Y", itemId="HZXXY3Y")
lists.suggesttip <- function(LIST_ID=NULL, itemId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/suggesttip"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Suggest Venues
#' 
#'  Suggests venues that may be appropriate for this list.
#' 
#' Suggests venues that may be appropriate for this list. 
#'	Note: Only valid on user-created lists
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created list
#' @param m Accepts values of "foursquare"
#' 
#' @return suggestedVenues An array of compact venues that may
#'	be appropriate for this list.
#' 
#' @examples
#' lists.suggestvenues(LIST_ID="HZXXY3Y")
lists.suggestvenues <- function(LIST_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("lists/",LIST_ID,"/suggestvenues"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Add an Item
#' 
#'  Allows you to add an item to a list.
#' 
#' Allows you to add an item to a list.  All fields are
#'	optional, but exactly one of the following must be
#'	specified:  venueId tipId listId and itemId  Note: This
#'	endpoint is now the preferred way to add an item to a
#'	user's To-do list (/v2/lists/USER_ID/todos/additem).
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created or followed
#'	list as well as one of either USER_ID/tips or
#'	USER_ID/todos.
#' @param venueId optional A venue to add to the list.
#' @param url optional If adding a new tip via text, this can
#'	associate a url with the tip.
#' @param tipId optional Used to add a tip to a list. Cannot be
#'	used in conjunction with the text and url fields.
#' @param listId optional Used in conjuction with itemId, the
#'	id for a user created or followed list as well as one of
#'	either USER_ID/tips or USER_ID/todos.
#' @param itemId optional Used in conjuction with listId, the
#'	id of an item on that list that we wish to copy to this
#'	list.
#' @param m Accepts values of "foursquare"
#' 
#' @return list item The list item that was just added.
#' 
#' @examples
#' lists.additem(LIST_ID="HZXXY3Y", venueId="HZXXY3Y",
#'	url="http://nyt.com", tipId="HZXXY3Y", listId="HZXXY3Y",
#'	itemId="HZXXY3Y")
lists.additem <- function(LIST_ID=NULL, venueId=NULL, url=NULL, tipId=NULL, listId=NULL, itemId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/additem"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Delete an Item
#' 
#'  Allows you to delete items from a list.
#' 
#' Allows you to delete items from a list. One of itemId,
#'	venueId, or tipId must be provided.  Note: Collaborators
#'	can only delete items they added. List owners may delete
#'	any item.
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created or followed
#'	list or one of either USER_ID/tips or USER_ID/todos.
#' @param itemId optional id of the item to delete.
#' @param venueId optional id of a venue to be deleted. If the
#'	venue is on the list multiple times, e.g. multiple tips at
#'	the same venue, all items will be removed.
#' @param tipId optional id of a tip to be deleted.
#' @param m Accepts values of "foursquare"
#' 
#' @return list items A count and items of list item that were
#'	just deleted.
#' 
#' @examples
#' lists.deleteitem(LIST_ID="HZXXY3Y", itemId="HZXXY3Y",
#'	venueId="HZXXY3Y", tipId="HZXXY3Y")
lists.deleteitem <- function(LIST_ID=NULL, itemId=NULL, venueId=NULL, tipId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/deleteitem"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Follow a List
#' 
#'  Allows you to follow a list.
#' 
#' Allows you to follow a list.  Note: Only valid on
#'	user-created lists.
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id of a user-created list.
#' @param m Accepts values of "foursquare"
#' 
#' @return list The list that was just followed.
#' 
#' @examples
#' lists.follow(LIST_ID="HZXXY3Y")
lists.follow <- function(LIST_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/follow"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Move Item
#' 
#'  Allows you to move an item on a list.
#' 
#' Allows you to move an item on a list. One of beforeId or
#'	afterId must be specified.  Note: Only valid on
#'	user-created lists
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id of a user-created list.
#' @param itemId required id of the item on this list to move.
#' @param beforeId optional Move itemId before beforeId.
#' @param afterId optional Move itemId after afterId.
#' @param m Accepts values of "foursquare"
#' 
#' @return list The list that was just edited.
#' 
#' @examples
#' lists.moveitem(LIST_ID="HZXXY3Y", itemId="HZXXY3Y",
#'	beforeId="HZXXY3Y", afterId="HZXXY3Y")
lists.moveitem <- function(LIST_ID=NULL, itemId=NULL, beforeId=NULL, afterId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/moveitem"),params=as.list( environment() ),requiresActingUsr="Yes")
}
lists.share <- function(LIST_ID=NULL, broadcast=NULL, message=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/share"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Unfollow a List
#' 
#'  Allows you to unfollow a list.
#' 
#' Allows you to unfollow a list.  Note: Only valid on
#'	user-created lists.
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id of a user-created list.
#' @param m Accepts values of "foursquare"
#' 
#' @return list The list that was just unfollowed.
#' 
#' @examples
#' lists.unfollow(LIST_ID="HZXXY3Y")
lists.unfollow <- function(LIST_ID=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/unfollow"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Updates a list
#' 
#'  Allows you to update a list.
#' 
#' Allows you to update a list.  Note: Only valid on
#'	user-created lists
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created list.
#' @param name If present and a non-empty value, updates the
#'	List name.
#' @param description If present and a non-empty value, updates
#'	the List description. If present and empty, will remove the
#'	List description.
#' @param collaborative Boolean indicating if this list can be
#'	edited by friends. Once this has been set to true for a
#'	list, authenticated friends can edit the  list via additem,
#'	deleteitem, etc.
#' @param photoId If present and a non-empty value, updates the
#'	List photo. If present and empty, will remove the List
#'	photo.
#' @param m Accepts values of "foursquare"
#' 
#' @return list The list that was just edited.
#' 
#' @examples
#' lists.update(LIST_ID="HZXXY3Y", name="Max's Favorite Ramen
#'	Spots", description="Must visit ramen spots according to
#'	Max.", collaborative="true", photoId="HZXXY3Y")
lists.update <- function(LIST_ID=NULL, name=NULL, description=NULL, collaborative=NULL, photoId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/update"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Update a List Item
#' 
#'  Allows you to add or remove photos and tips.
#' 
#' Allows you to add or remove photos and tips from items on
#'	user-created lists.  Note: Only valid on user-created lists
#'	 Note: Collaborators can only update items they added. List
#'	owners can not update any item.
#' 	Note: This function requires acting user
#' 
#' @param LIST_ID required id for a user-created list.
#' @param itemId required The id of an item on this list.
#' @param tipId optional If present and a non-empty value, adds
#'	or replaces a tip on this item. If present and empty, will
#'	remove the tip on this item.
#' @param text optional If present creates a public tip on the
#'	venue and replaces any existing tip on the item. Cannot be
#'	used in conjuction with tipId or photoId
#' @param url optional If adding a new tip via text, this can
#'	associate a url with the tip.
#' @param photoId optional If present and a non-empty value,
#'	adds a photo to this item. If present and empty, will
#'	remove the photo on this item. If the photo was a private
#'	checkin photo, it will be promoted to a public venue photo.
#' @param m Accepts values of "foursquare"
#' 
#' @return list item The updated list item.
#' 
#' @examples
#' lists.updateitem(LIST_ID="HZXXY3Y", itemId="HZXXY3Y",
#'	tipId="HZXXY3Y", text="This ramen spot is the jam",
#'	url="http://nyt.com", photoId="HZXXY3Y")
lists.updateitem <- function(LIST_ID=NULL, itemId=NULL, tipId=NULL, text=NULL, url=NULL, photoId=NULL,m="foursquare"){
  fq_POST(end_point=paste0("lists/",LIST_ID,"/updateitem"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Update detail
#' 
#'  Update detail
#' 
#' Update detail
#' 	Note: This function requires acting user
#' 
#' @param UPDATE_ID required The ID of the update to retrieve
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return notification A update.
#' 
#' @examples
#' updates.updates(UPDATE_ID="BTEP567")
updates.updates <- function(UPDATE_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("updates/",UPDATE_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Notification tray
#' 
#'  Retrieve a user's notification tray notifications.
#' 
#' Retrieve a user's notification tray notifications
#' 	Note: This function requires acting user
#' 
#' @param limit Maximum number of results to return, up to 99.
#'	Notifications are grouped over time, so there will usually
#'	be fewer than 99 results available at any given time.
#'	offset 0 Used to page through results. Only the 99 most
#'	recent notifications are visible, so offset must be no more
#'	than 99 - limit.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return notifications The user's notification updates.
#' 
#' @examples
#' updates.notifications(limit="20")
updates.notifications <- function(limit=NULL,m="foursquare"){
  fq_GET(end_point="updates/notifications",params=as.list( environment() ),requiresActingUsr="Yes")
}
updates.marknotificationsread <- function(highWatermark=NULL,m="foursquare"){
  fq_POST(end_point="updates/marknotificationsread",params=as.list( environment() ),requiresActingUsr="Yes")
}


#' Photo details
#' 
#'  Get details of a photo. 
#' 
#' Get details of a photo.
#' 	Note: This function requires acting user
#' 
#' @param PHOTO_ID required The ID of the photo to retrieve
#'	additional information for.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return photo A complete photo object.
#' 
#' @examples
#' photos.photos(PHOTO_ID="4d0fb8162d39a340637dc42b")
photos.photos <- function(PHOTO_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("photos/",PHOTO_ID,"/photos"),params=as.list( environment() ),requiresActingUsr="Yes")
}
#photos.photos <- function(PHOTO_ID=NULL,m="foursquare"){
#  fq_GET(end_point=paste0("photos/",PHOTO_ID,"/photos"),params=as.list( environment() ),requiresActingUsr="Yes")
#}
##' Add a Photo
##' 
##'  Allows users to add a new photo to a checkin, tip, venue, etc.
##' 
##' Allows users to add a new photo to a checkin, tip, venue, or
##'	page update in general.  All fields are optional, but
##'	exactly one of the id fields (checkinId, tipId, venueId,
##'	pageId) must be passed in.  In addition, the image file
##'	data must be posted. The photo should be uploaded as a jpeg
##'	and the Content-Type should be set to "image/jpeg". Images
##'	may not exceed 5MB in size.  Attaching a photo to a tip or
##'	a venue makes it visible to anybody. By default, attaching
##'	a photo to a checkin makes it visible only to the people
##'	who can see the checkin (the user's friends, unless the
##'	checkin has been sent to Twitter or Facebook); this can be
##'	overridden by specifying public=1 in the request. 
##'	Attaching a photo to a page is visible to anybody and is
##'	intended for use only in an update.  Multiple photos can be
##'	attached to a checkin, venue, or page, but there can only
##'	be one photo per tip.  To avoid double-tweeting, if you are
##'	sending a checkin that will be immediately followed by a
##'	photo, do not set broadcast=twitter on the checkin, and
##'	just set it on the photo.  When adding a photo to a
##'	checkin, specify the post* params to connect the photo back
##'	to the corresponding content inside your app. For more
##'	information, see our documentation on native links.
##' 	Note: This function requires acting user
##' 
##' @param checkinId the ID of a checkin owned by the user.
##' @param tipId the ID of a tip owned by the user.
##' @param venueId the ID of a venue, provided only when adding
##'	a public photo of the venue in general, rather than a photo
##'	for a private checkin, tip, or page update.
##' @param pageId the ID of a page, provided only when adding a
##'	photo that will be in an update for that page (no other ids
##'	should be specified).
##' @param broadcast Whether to broadcast this photo. Send
##'	twitter if you want to send to twitter, facebook if you
##'	want to send to facebook, or twitter,facebook if you want
##'	to send to both.
##' @param public When the checkinId is also provided (meaning
##'	this is a photo attached to a checkin), this parameter
##'	allows for making the photo public and viewable at the
##'	venue. Valid values are 1 and 0 (default).Note that photos
##'	attached to venues, tips, and pages (updates) are always
##'	public.
##' @param ll Latitude and longitude of the user's location.
##' @param llAcc Accuracy of the user's latitude and longitude,
##'	in meters.
##' @param alt Altitude of the user's location, in meters.
##' @param altAcc Vertical accuracy of the user's location, in
##'	meters.
##' @param postUrl A link for more details about the photo. This
##'	page will be opened in an embedded web view in the
##'	foursquare application, unless contentId is specified and a
##'	native link handler is registered and present. We support
##'	the following URL schemes: http, https, foursquare, mailto,
##'	tel, and sms.
##' @param postContentId Identifier for the photo post to be
##'	used in a native link, up to 50 characters. A checkinId and
##'	postUrl must also be specified in the request.
##' @param postText Text for the photo post, up to 200
##'	characters. A checkinId must also be specified in the
##'	request.
##' @param m Accepts values of "foursquare" or "swarm"
##' 
##' @return photo The photo that was just created.
##' 
##' @examples
##' photos.add(checkinId="IHR8THISVNU", tipId="IHR8THISVNU",
##'	venueId="IHR8THISVNU", pageId="2345", broadcast="twitter",
##'	public="1", ll="33.7,44.2", llAcc="1", alt="0", altAcc="1",
##'	postUrl="http://your.site.com/path",
##'	postContentId="3ER4GN93AFWE", postText="Awesome!")
#photos.add <- function(checkinId=NULL, tipId=NULL, venueId=NULL, pageId=NULL, broadcast=NULL, public=NULL, ll=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, postUrl=NULL, postContentId=NULL, postText=NULL,m="foursquare"){
#  fq_POST(end_point="photos/add",params=as.list( environment() ),requiresActingUsr="Yes")
#}

#' Setting detail
#' 
#'  Returns a setting for the acting user.                     
#' 
#' Returns a setting for the acting user.
#' 	Note: This function requires acting user
#' 
#' @param SETTING_ID The name of a setting.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return value The value for this setting for the acting
#'	user.
#' 
#' @examples
#' settings.settings(SETTING_ID="receivePings")
settings.settings <- function(SETTING_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("settings/",SETTING_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
settings.all <- function(m="foursquare"){
  fq_GET(end_point="settings/all",params=as.list( environment() ),requiresActingUsr="Yes")
}


#' Change a setting
#' 
#'  Change a setting for the given user. 
#' 
#' Change a setting for the given user.
#' 	Note: This function requires acting user
#' 
#' @param SETTING_ID required Name of setting to change,
#'	sendMayorshipsToTwitter, sendBadgesToTwitter,
#'	sendMayorshipsToFacebook, sendBadgesToFacebook,
#'	receivePings, receiveCommentPings.
#' @param value required 1 for true, and 0 for false.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return message A confirmation message.
#' 
#' @examples
#' settings.set(SETTING_ID="sendBadgesToTwitter", value="1")
settings.set <- function(SETTING_ID=NULL, value=NULL,m="foursquare"){
  fq_POST(end_point=paste0("settings/",SETTING_ID,"/set"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Special Detail
#' 
#'  Gives details about a special.
#' 
#' Gives details about a special, including text and whether it
#'	is unlocked for the current or provided user.
#' 
#' @param SPECIAL_ID required ID of special to retrieve
#' @param venueId required ID of a venue the special is running
#'	at
#' @param userId ID of the user to check whether the special is
#'	unlocked for. Only available if the current user is the
#'	manager of the venue. If not provided, checks wher the
#'	special is unlocked for the current user.
#' @param m Accepts values of "foursquare"
#' 
#' @return special A complete special.
#' 
#' @examples
#' specials.specials(SPECIAL_ID="A9ABCD", venueId="B92CD1",
#'	userId="2462")
specials.specials <- function(SPECIAL_ID=NULL, venueId=NULL, userId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("specials/",SPECIAL_ID),params=as.list( environment() ),requiresActingUsr="No")
}

#' Create a new special
#' 
#'  Allows you to create a new special.
#' 
#' Allows you to create a new special. As of November 2013,
#'	mayor, regular, swarm, friends, and flash specials are
#'	deprecated.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param name A name for the special.
#' @param text Required. Maximum length of 200 characters.
#' @param finePrint Maximum length of 200 characters. Fine
#'	print, shown in small type on the special detail page.
#' @param count1 Specifier for special types.
#' @param type Required. The type of special. 
#'	frequencyCheck-in Special if count1 is 1; otherwise a
#'	Loyalty Specialunlocked every count1 check-ins countNewbie
#'	Special if count1 is 1; otherwise a Loyalty Specialunlocked
#'	on the count1th check-in (all-time) 
#' @param offerId Maximum length of 16 characters. Internal id
#'	in your 3rd party system.
#' @param cost The amount of money the user must spend to use
#'	this special in dollars and cents. For example, 5.50
#'	meaning 5 dollars and 50 cents.
#' @param m Accepts values of "foursquare"
#' 
#' @return special A special object.
#' 
#' @examples
#' specials.add(name="Mayor Special", text="Congrats, as the
#'	Mayor, you get a free side!", finePrint="Does not include
#'	fries.", count1="5", type="frequency", offerId="5000000",
#'	cost="5.50")
specials.add <- function(name=NULL, text=NULL, finePrint=NULL, count1=NULL, type=NULL, offerId=NULL, cost=NULL,m="foursquare"){
  fq_POST(end_point="specials/add",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' List Specials
#' 
#'  List available specials. 
#' 
#' List available specials.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param venueId comma-separated list of venue IDs; filters
#'	results to the specials  assigned to the venue(s).
#' @param status (optional, requires venueId) which specials to
#'	return: pending, active, expired, all
#' @param m Accepts values of "foursquare"
#' 
#' @return specials A count and items of specials.
#' 
#' @examples
#' specials.list(venueId="IHR8THISVNU", status="active")
specials.list <- function(venueId=NULL, status=NULL,m="foursquare"){
  fq_GET(end_point="specials/list",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Search Specials
#' 
#'  Returns a list of specials near the current location. 
#' 
#' Returns a list of specials near the current location.
#' 
#' @param ll Required. Latitude and longitude to search near.
#' @param radius Limit results to venues within this many
#'	meters of the specified location. Defaults to a city-wide
#'	area.
#' @param llAcc Accuracy of latitude and longitude, in meters.
#' @param alt Altitude of the user's location, in meters.
#' @param altAcc Accuracy of the user's altitude, in meters.
#' @param limit Number of results to return, up to 50.
#' @param m Accepts values of "foursquare"
#' 
#' @return specials An array of specials being run at nearby
#'	venues. This array is identical in structure to the nearby
#'	specials array returned by the venue detail endpoint. Each
#'	special in the returned array contains a compact venue.
#' 
#' @examples
#' specials.search(ll="44.3,37.2", radius="800",
#'	llAcc="10000.0", alt="0", altAcc="10000.0", limit="10")
specials.search <- function(ll=NULL, radius=NULL, llAcc=NULL, alt=NULL, altAcc=NULL, limit=NULL,m="foursquare"){
  fq_GET(end_point="specials/search",params=as.list( environment() ),requiresActingUsr="No")
}
specials.flag <- function(ID=NULL, venueId=NULL, problem=NULL, text=NULL,m="foursquare"){
  fq_POST(end_point=paste0("specials/",ID,"/flag"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Event details
#' 
#'  Get details of a event. 
#' 
#' Get details of a event.
#' 	Note: This function requires acting user
#' 
#' @param EVENT_ID required The ID of the event to retrieve
#'	additional information for.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return event A complete event object.
#' 
#' @examples
#' events.events(EVENT_ID="4d0fb8162d39a340637dc42b")
events.events <- function(EVENT_ID=NULL,m="foursquare"){
  fq_GET(end_point=paste0("events/",EVENT_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}
events.categories <- function(m="foursquare"){
  fq_GET(end_point="events/categories",params=as.list( environment() ),requiresActingUsr="No")
}


#' Search Events
#' 
#'  Experimental
#' 
#' This is an experimental API and subject to change or
#'	breakage.  Returns a list of events matching the search
#'	parameters.
#' 
#' @param domain required Identifier for a known third-party
#'	event provider. This is used in conjunction with id.
#'	Currently songkick.com is the only supported value.
#' @param eventId_or_participantId required Identifier used by
#'	third-party specifed in domain, which we will attempt to
#'	match against our events listings. eventId is the id of an
#'	event, participantId is the id of a participant, like a
#'	band or sports team.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return event An array of compact events.
#' 
#' @examples
#' events.search(domain="songkick.com",
#'	eventId_or_participantId="8183976")
events.search <- function(domain=NULL, eventId=NULL, participantId=NULL,m="foursquare"){
  fq_GET(end_point="events/search",params=as.list( environment() ),requiresActingUsr="No")
}

#' Add an event
#' 
#'  Create an event for a venue that you manage. 
#' 
#' Create an event for a venue that you manage. You can see all
#'	your events in the tools tab when you're managing your
#'	venue.  They're on the calendar.  Events show up when users
#'	view your venue on mobile or the web. Users can also check
#'	in to events and share with their friends what they're up
#'	to in the present moment, and look back to see what they
#'	did in the days of yore.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param venueId The id of the venue where the event is being
#'	held.
#' @param name The name of the event.
#' @param startAt Time when the event is scheduled to start, in
#'	seconds since Unix epoch.
#' @param endAt Time when the event is scheduled to end, in
#'	seconds since Unix epoch.
#' @param m Accepts values of "foursquare" or "swarm"
#' 
#' @return event An event object.
#' 
#' @examples
#' events.add(venueId="f8ea99941e", name=""Trivia Night!"",
#'	startAt="1355469600", endAt="1355470000")
events.add <- function(venueId=NULL, name=NULL, startAt=NULL, endAt=NULL,m="foursquare"){
  fq_POST(end_point="events/add",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Add a Page
#' 
#'  Allows users to create a new page.
#' 
#' Allows users to create a new page.  The creating user is
#'	added as a manager of the new page.
#' 	Note: This function requires acting user
#' 
#' @param name required the name of the page
#' @param m Accepts values of "foursquare"
#' 
#' @return user The user account for the page that was just
#'	created. An OAuth access token for this page for the acting
#'	OAuth consumer.
#' 
#' @examples
#' pages.add(name="Habana Outpost")
pages.add <- function(name=NULL,m="foursquare"){
  fq_POST(end_point="pages/add",params=as.list( environment() ),requiresActingUsr="Yes")
}
pages.managing <- function(m="foursquare"){
  fq_GET(end_point="pages/managing",params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Access Token
#' 
#'  Returns an OAuth access token for the specified page.
#' 
#' Returns an OAuth access token for the specified page.  The
#'	requesting user must be a manager of the page.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID required The page you want similar pages for.
#' @param m Accepts values of "foursquare"
#' 
#' @return access_token An OAuth access token for this page for
#'	the acting OAuth consumer.
#' 
#' @examples
#' pages.access(USER_ID="HZXXY3")
pages.access <- function(USER_ID="self",m="foursquare"){
  fq_GET(end_point=paste0("pages/",USER_ID,"/access"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Similar Pages
#' 
#'  Returns a list of pages similar to the specified page.  
#' 
#' Returns a list of pages similar to the specified page.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID required The page you want similar pages for.
#' @param limit Number of results to return, up to 500.
#' @param offset Used to page through results.
#' @param includeFollowing optional Boolean indicating whether
#'	results include pages already being followed by the user.
#' @param m Accepts values of "foursquare"
#' 
#' @return similarPages A count and items array. Each element
#'	in items contains a pageInfo compact page, a user compact
#'	user, Both followers and tips contain a count and tips may
#'	also contain an array of selected tips as items.
#' 
#' @examples
#' pages.similar(USER_ID="HZXXY3", limit="20", offset="100",
#'	includeFollowing="true")
pages.similar <- function(USER_ID="self", limit=NULL, offset=NULL, includeFollowing=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pages/",USER_ID,"/similar"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Page Venues Time Series Data
#' 
#'  Get daily venue stats for venues managed by a page.
#' 
#' Get daily venue stats for venues managed by a page over a
#'	time range.
#' 	Note: This function requires acting user
#' 	User restrictions: Must be venue manager 
#' 
#' @param PAGE_ID The page whose venues to get timeseries data
#'	for
#' @param startAt required. The start of the time range to
#'	retrieve stats for (seconds since epoch).
#' @param endAt The end of the time range to retrieve stats for
#'	(seconds since epoch). If omitted, the current time is
#'	assumed.
#' @param fields Specifies which fields to return. May be one
#'	or more of totalCheckins, newCheckins, uniqueVisitors,
#'	sharing, genders, ages, hours, separated by commas.
#' @param m Accepts values of "foursquare"
#' 
#' @return timeseries An array of venue time series data
#'	objects, one for each venue the page manages.
#' 
#' @examples
#' pages.timeseries(PAGE_ID="2345", startAt="1284286794",
#'	endAt="1284286794", fields="totalCheckins,newCheckins")
pages.timeseries <- function(PAGE_ID=NULL, startAt=NULL, endAt=NULL, fields=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pages/",PAGE_ID,"/timeseries"),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Page Venues
#' 
#'  Allows you to get the page's venues. 
#' 
#' Allows you to get the page's venues.
#' 
#' @param PAGE_ID required The page id for which venues are
#'	being requested.
#' @param ll optional Not valid with ne or sw. Limits results
#'	to venues near this latitude and longitude within an
#'	optional radius.
#' @param radius optional Can be used when including ll.  Not
#'	valid with ne or sw. Limit results to venues within this
#'	many meters of the specified ll. The maximum supported
#'	radius is currently 100,000 meters.
#' @param sw With ne, limits results to the bounding quadrangle
#'	defined by the latitude and longitude given by sw as its
#'	south-west corner, and ne as its north-east corner. Not
#'	valid with ll or radius. Bounding quadrangles with an area
#'	up to approximately 10,000 square kilometers are supported.
#' @param ne See sw
#' @param offset The offset of which venues to return. Defaults
#'	to 0.
#' @param limit The number of venues to return. Defaults to 20,
#'	max of 100.
#' @param storeId optional Only return venues whose storeId
#'	matches. storeIds are defined by the page manager (and
#'	therefore namespaced to that particular page). They are the
#'	page's own internal identifier for that venue. Cannot be
#'	used with any geo params.
#' @param m Accepts values of "foursquare"
#' 
#' @return venues A count and items of compact venues.
#' 
#' @examples
#' pages.venues(PAGE_ID="AVNU234", ll="44.3,37.2",
#'	radius="800", sw="44.3,37.2", ne="44.1,37.4", offset="50",
#'	limit="30", storeId="995")
pages.venues <- function(PAGE_ID=NULL, ll=NULL, radius=NULL, sw=NULL, ne=NULL, offset=NULL, limit=NULL, storeId=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pages/",PAGE_ID,"/venues"),params=as.list( environment() ),requiresActingUsr="No")
}

#' Follow or unfollow a page.
#' 
#'  Allows the acting user to follow or unfollow a page.
#' 
#' Allows the acting user to follow or unfollow a page.
#'	Following a page subscribes the acting user to updates from
#'	that page.
#' 	Note: This function requires acting user
#' 
#' @param USER_ID required The page to follow or unfollow.
#' @param set If 1, follow this page. If 0 unfollow (un-do a
#'	previous follow) it. Default value is 1.
#' @param m Accepts values of "foursquare"
#' 
#' @return user A user object representing the page the acting
#'	user has just followed or unfollowed.
#' 
#' @examples
#' pages.follow(USER_ID="XXX123YYYY", set="1")
#pages.follow <- function(USER_ID="self", set=NULL,m="foursquare"){
#  fq_POST(end_point=paste0("pages/",USER_ID,"/follow"),params=as.list( environment() ),requiresActingUsr="Yes")
#}

#' Page Update Details
#' 
#'  Get page update details. 
#' 
#' Get page update details.
#' 	Note: This function requires acting user
#' 
#' @param UPDATE_ID The ID of the update to retrieve additional
#'	information for.
#' @param limit The number of nearby venues to show (default
#'	20).
#' @param ll The location of the current user.
#' @param m Accepts values of "foursquare"
#' 
#' @return pageUpdate A page update object.
#' 
#' @examples
#' pageupdates.pageupdates(UPDATE_ID="23456", limit="10",
#'	ll="40.74,-74.0")
pageupdates.pageupdates <- function(UPDATE_ID=NULL, limit=NULL, ll=NULL,m="foursquare"){
  fq_GET(end_point=paste0("pageupdates/",UPDATE_ID),params=as.list( environment() ),requiresActingUsr="Yes")
}

#' Add a Page Update
#' 
#'  Broadcast an update as a page to followers of the page.
#' 
#' Broadcast an update as a page to followers of the page and
#'	associated venues. Venues can be specified either by the
#'	venueId, groupId or pageId (meaning all venues managed by
#'	the page) parameters. Broadcasts will show up on venue
#'	pages in our apps and website.  A broadcast must contain a
#'	shout and may contain a photo. The API enforces a rate
#'	limit of at most one broadcast sent by a given user to the
#'	same set of venues in 15 minutes. This rate may change and
#'	is stricter for larger sets of venues.
#' 	Note: This function requires acting user
#' 
#' @param pageId required id of the page to associate with the
#'	broadcast.  To find the page for a venue you can look at
#'	the page object in the venue response.
#' @param groupId The venue group from which to broadcast an
#'	update.
#' @param venueId A venue from which to broadcast an update.
#' @param shout Text associated with the broadcast. 160
#'	characters max, 10 characters min.
#' @param photoId An optional photo to attach to the broadcast.
#'	 For a new photo, you should use the photo add endpoint and
#'	specify only the pageId parameter.
#' @param broadcast Additional places to send the broadcast to.
#'	Accepts a comma-delimited list of values:  facebook share
#'	on facebook twitter share on twitter private just create
#'	the update without broadcasting to anyone 
#' @param m Accepts values of "foursquare"
#' 
#' @return pageUpdate A page update object.
#' 
#' @examples
#' pageupdates.add(pageId="23456", groupId="f8ea99941e",
#'	venueId="f8ea99941e", shout=""Try our new summer dish!"",
#'	photoId="f8ea99941e", broadcast="twitter")
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
