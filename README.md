Rfoursquare
===========

R wrapper for the [foursquare v2 API](http://developer.foursquare.com/docs/). 

Philosophy:

* Map foursquare's endpoints one-to-one
* Clean, simple, R calls

Features:

* OAuth dance
* Full endpoint coverage


Dependencies:

* httr
* jsonlite

## Usage

### Authentication

    # Initialize foursquare OAuth
    initFoursquare( CLIENT_ID="XXXXXX", 
                    CLIENT_SECRET="XXXXXX", 
                    ACCESS_TOKEN="XXXXXX")


### Examples

#### Users
##### [Getting your own user object](https://developer.foursquare.com/docs/users/users)
    users.users()
##### [Getting another user](https://developer.foursquare.com/docs/users/users)
    users.users(USER_ID='1183247')
##### [Get your checkins](https://developer.foursquare.com/docs/users/checkins)
    users.checkins()
##### [Get your most recent checkin](https://developer.foursquare.com/docs/users/checkins)
    users.checkins(limit="1")
##### [Approve a friend's friend request](https://developer.foursquare.com/docs/users/approve)
    users.approve(USER_ID="self",'1183247')

#### Venues
##### [Get details about a venue](https://developer.foursquare.com/docs/venues/venues)
    venues.venues(VENUE_ID='40a55d80f964a52020f31ee3')
##### [Search for a coffee place](https://developer.foursquare.com/docs/venues/search)
    venues.search(near = 'New york', query = 'coffee')
##### [Edit venue details](https://developer.foursquare.com/docs/venues/edit)
    venues.edit(VENUE_ID='40a55d80f964a52020f31ee3', description='Best restaurant on the city')

#### Checkins
##### [Get recent checkins for yourself and your friends](https://developer.foursquare.com/docs/checkins/recent)
    checkins.recent()

#### Tips
##### [Get a specific tip](https://developer.foursquare.com/docs/tips/tips)
    tips.tips(TIP_ID='4b5e662a70c603bba7d790b4')


### Full endpoint list
Note: endpoint methods map one-to-one with foursquare's endpoints

    users.users(USER_ID = "XXXXXX")
    users.requests()
    users.search(name = "tiesto")
    users.checkins()
    users.friends()
    users.lists()
    users.mayorships()
    users.photos()
    users.tips()
    users.venuehistory()
    users.venuelikes()
    users.approve()
    users.deny()
    users.setpings(USER_ID = "XXXXXX",value = FALSE)
    users.unfriend(USER_ID = "XXXXXX")
    [NOT IMPLEMENTED] users.update()
    venues.venues(VENUE_ID = "50c10b72e4b0dbf359f7d91b")
    venues.add() 
    venues.categories()
    venues.explore(ll = "19.3422177,-99.1990016", radius = 50)
    venues.managed()
    venues.search(ll = "19.3422177,-99.1990016")
    venues.suggestcompletion(ll = "19.3422177,-99.1990016",query="museo")
    venues.timeseries() 
    venues.trending(ll = "19.3422177,-99.1990016")
    venues.events(VENUE_ID = "50c10b72e4b0dbf359f7d91b")
    venues.herenow(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.hours(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.likes(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.links(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.listed(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.menu(VENUE_ID = "50c10b72e4b0dbf359f7d91b")
    venues.nextvenues(VENUE_ID = "50c10b72e4b0dbf359f7d91b")
    venues.photos(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.similar(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.stats(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.tips(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.claim() 
    venues.dislike()
    venues.edit()
    venues.flag()
    venues.like(VENUE_ID = "4b8dce90f964a5207f0e33e3")
    venues.proposeedit()
    venues.setrole() 
    venues.setsinglelocation()
    venuegroups.venuegroups() 
    venuegroups.add() 
    venuegroups.delete() 
    venuegroups.list()
    venuegroups.timeseries() 
    venuegroups.addvenue()
    venuegroups.edit() 
    venuegroups.removevenue() 
    venuegroups.update()
    checkins.checkins(CHECKIN_ID = "YYYYYY")
    checkins.add(venueId = "4b8dce90f964a5207f0e33e3")
    checkins.recent(ll = "19.3422177,-99.1990016")
    checkins.likes(CHECKIN_ID = "YYYYYY")
    checkins.addcomment(CHECKIN_ID = "YYYYYY", text = "ola ke ase, en dispositivos moviles o ke ase")
    checkins.addpost()
    checkins.deletecomment()
    checkins.like(CHECKIN_ID = "YYYYYY", set = '1')
    tips.tips(TIP_ID = "4f9c3925e4b0614492d5046d")
    tips.add(venueId = "4b8dce90f964a5207f0e33e3", text = "Tenemos un sphero! Laboratorio de Usabilidad y Sistemas Interactivos (LUSI) en ITAM")
    tips.likes(TIP_ID = "542afce4498ef9a9bc552f73")
    tips.listed(TIP_ID = "4f9c3925e4b0614492d5046d")
    tips.saves(TIP_ID = "4f9c3925e4b0614492d5046d")
    tips.flag()  
    tips.like(TIP_ID = "4f9c3925e4b0614492d5046d", set = 1)
    tips.unmark()  
    lists.lists(USER_ID = "self", defaultList = "todos")
    lists.add(name="", description = "") 
    lists.followers(LIST_ID = "542b1bad498e9dd771c21740")
    lists.items(LIST_ID = "542b1bad498e9dd771c21740")
    lists.saves(LIST_ID = "542b1bad498e9dd771c21740")
    lists.suggestphoto(LIST_ID = "542b1bad498e9dd771c21740", itemId = "v4d75a3b3d94ea35db781aaaf")
    lists.suggesttip(LIST_ID = "542b1bad498e9dd771c21740", itemId = "v4d75a3b3d94ea35db781aaaf")
    lists.suggestvenues(LIST_ID = "542b1bad498e9dd771c21740")
    lists.additem(LIST_ID = "542b1bad498e9dd771c21740",venueId = "4d75a3b3d94ea35db781aaaf") 
    lists.deleteitem(LIST_ID = "542b1bad498e9dd771c21740",itemId = "t4f9c3925e4b0614492d5046d")
    lists.follow()  
    lists.moveitem()
    lists.share() 
    lists.unfollow() 
    lists.update()
    lists.updateitem()
    updates.updates(UPDATE_ID = "5420f058498e7ede906008b0")
    updates.notifications()
    updates.marknotificationsread(highWatermark = "1411444768")
    [NOT IMPLEMENTED] photos.photos() 
    [NOT IMPLEMENTED] photos.add() 
    settings.settings(SETTING_ID = 'receivePings')
    settings.all()
    settings.set(SETTING_ID = 'receivePings',value = 1)
    specials.specials(SPECIAL_ID = "52e95e9f498e027ce7697343")
    specials.add()  
    specials.list() 
    specials.search(ll = "19.3422177,-99.1990016")
    specials.flag() 
    events.events(EVENT_ID = "4e173d2cbd412187aabb3c04")
    events.categories()
    events.search(domain = "songkick.com",eventId="8183976")
    events.add() 
    pages.add() 
    pages.managing()
    pages.access(USER_ID = "32834042")
    pages.similar(USER_ID = "32834042")
    pages.timeseries(PAGE_ID = "32834042")
    pages.venues(PAGE_ID = "32834042")
    [NOT IMPLEMENTED] pages.follow(USER_ID = "32834042", set="0") 
    pageupdates.pageupdates() 
    pageupdates.add() 
    pageupdates.list() 
    pageupdates.delete() 
    pageupdates.like() 


## Improvements
What else would you like this library to do? Let me know. Feel free to send pull requests for any improvements you make.
Send me comments if you find bugs

### TODO
* Bring in new endpoints as they emerge
* Test coverage for write methods
* Implement file uploading
