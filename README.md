# Beijing 2022 Data Repository

To get to the CSVs or JSONs, look in the *Data* Folder. The individual files are named after the matchIDs. I'm hoping this will make it easier to make into an R Package. To get the key for these matchIDs, look in the *Match ID Keys Folder*.

The R files that I left not in a folder are the ones used to originally pull the data from the NBC Olympics Website. Hopefully those will come in handy for Paris 2024 when that comes around (particularly the get_summer_api_sport.R, I think that gives me the sportID for those sports minus the new ones like Breaking). Y'all already know what's in the Output Folder. The Parsing Files are what I used to convert from the JSON Files to CSVs so if there's any issues with the CSVs, it's probably in that. Scraper Files folder is what I used to get the raw JSON files downloaded rather than pinging their API over and over and potentially getting banned. Finally, Test Files are what I used to get my data for the Beijing 2022 Olympics Graphs. These are *rough*, but you might be able to find some use for them in digging into the API to get some data you want that isn't in the CSVs yet.

Last Updated: May 5, 2022 at 12:58 AM

