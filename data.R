
library(spotifyr)


# client ID and client secret defined elsewhere
# same with spotify_id
access_token <- get_spotify_access_token(client_id = client_id,
                                         client_secret = client_secret)
auth_code <- get_spotify_authorization_code(client_id = client_id,
                                            client_secret = client_secret)

# my_plists <- get_user_playlists(spotify_id,
#                                 authorization = auth_code)

# Get the Weeknd's albums
get_artist_albums('1Xyo4u8uXC1ZmMpatF05PJ')

