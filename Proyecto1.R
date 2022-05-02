library(secret)
library(spotifyr)   
library(dplyr)
library(purrr)
library(knitr)
library(httr)       
library(tidyverse)  
library(cluster)    
library(factoextra) 
library(data.table)
#Loreto Aranda

clientID = "b3821a1fc03e41b2b5ea14e71f1c6192"
secret = "763f96a92d1b4a22afc2c4915e7abc56"

response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

token = content(response)$access_token
authorization.header = paste0("Bearer ", token)



Cancion = readline(prompt="Ingrese nombre de la cancion que quiere utilizar: ")

search_track = search_spotify(Cancion, type = c("track"),
                              market = NULL, limit = 20, offset = 0, include_external = NULL,
                              authorization = token,
                              include_meta_info = FALSE)
busqueda = subset(search_track, select= c("name", "album.name", "uri", "id"))
print(busqueda)

#search_filtered = subset(search_track, select= c("name", "album.name", "uri"))
#selected_track = "0OiXVxQHUFxvYxQew4xyhD" #se escoge el uri de la cancion que queremos utilizar
selected<- readline(prompt = "Seleccione numero de la canción de acuerdo a la tabla recien vista: ")#
Cancion= busqueda[selected,1:4]
selected_id = pull(Cancion, var=4)
selected_uri = pull(Cancion, var=3)


temp_features = get_track_audio_features(selected_id, authorization = token)


track_features = subset(temp_features, select= c("uri","danceability","duration_ms", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "tempo", "valence"))
 
#archivo con canciones que nos dieron 
beats <- readRDS("beats.rds")
#reduccion de columnas 
test <- subset(beats, select= c("track_uri", "danceability","duration_ms", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "tempo", "valence"))
#cambio de nombre para poder realizar un merge mas adelante
names(test)[1] <- 'uri'
#eliminar los datos duplicados, si existiesen 
test <- unique(test, by = "uri")

#unimos
new <- rbind(test, track_features)
rownames(new) <- new$uri

#creacion de df con los datos
df <- scale(new[,2:10])
#modelo 1
#prueba de clusters con K-Means
#https://www.rdocumentation.org/packages/factoextra/versi
#k <- kmeans(df, centers = 2, nstart = 25)
#fviz_cluster(k, geom = "point",  data = df) + ggtitle("k = 2")
#k6 <- kmeans(df, centers = 6, nstart = 25)
#fviz_cluster(k6, geom = "point",  data = df) + ggtitle("k = 6")
#Se puede identificar que al aumentar el numero de clusters, estos se comienzan a sobreponer unos sobre otros. 
#Al mantener solo 2 cluster podemos ver que podriamos separar las canciones en dos tipos, pero esto no seria preciso sin realizar
#un estudio a mayor detalle. K-means no es el mas adecuado para esto, debido a la alta cantidad de datos y su complejidad.
#ahora creamos una lista de canciones 

#kmeans_tracks=order(k$cluster)
#kmeans_data = data.frame(new$uri[kmeans_tracks],k$cluster[kmeans_tracks])

#selected_cluster = kmeans_data[kmeans_data$new.uri.kmeans_tracks.== selected_track, 2]


#lista_data = kmeans_data[kmeans_data$k.cluster.kmeans_tracks.== selected_cluster, ]
#no sigo adelante con esta lista, ya que no es el modelo seleccionado. 

#modelo 2
#utilizare solo una muestra de los datos, decidi quedarme con este modelo gracias a la forma que tiene de mostrar los datos
# y la facilidad de manejar grandes cantidades de datos. 
set.seed(42) 
df2 = df[sample(nrow(df), 1000), ]
df2<- rbind(df2, df[Cancion$uri,]) 
row.names(df2)[1001] <- track_features$uri
distancia <- dist(df2, method = "euclidean")
agnes <- agnes(df2)

fviz_nbclust(df2, FUN = hcut, method = "silhouette")#esto nos permite identificar la cant optima de clusters

agnes_ <- hclust(distancia, method = "ward.D" )
new_agnes <- cutree(agnes_, k = 2)
table(new_agnes)

nuevo_df = data.frame(keyName=names(new_agnes), value=new_agnes, row.names=NULL)
cluster_tracks=order(nuevo_df$value)


cluster_data = data.frame(row.names(df2),nuevo_df$value[cluster_tracks])
selected_cluster = cluster_data[1001, 2] #escogi quedarme con que cluster quedarme
#selecciono 55 canciones, lo que podria ser cercano a 3 hrs
playlist_data = cluster_data[cluster_data$nuevo_df.value.cluster_tracks.== selected_cluster, ]
new_playlist = playlist_data[sample(nrow(playlist_data), 55), ]

View(new_playlist) #lista de canciones similares a la cancion ingresada, esta lista es exportable y se puede subir a spotify
write.table(new_playlist, "PlaylistNueva") #muesta el numero de la cancion en la data que nos dieron, el URI con el que se puede buscar la cancion
#y el cluster al que pertenece


#creando lista en spotify
PlaylistNueva = readline(prompt="Ingrese nombre la playlist a crear: ")
user_id <- readline(prompt = "Ingrese su user id, ingresar URI string: ")
auth_token <- get_spotify_authorization_code(scope = scope)


#https://www.rdocumentation.org/packages/spotifyr/versions/2.1.1/topics/create_playlist

playlist = create_playlist(user_id,PlaylistNueva, public = TRUE, collaborative = FALSE,
                           description = NULL, authorization = get_spotify_authorization_code())

playlist_vector = as.character(new_playlist[,1])
uris = paste0('\"',playlist_vector, collapse = '\",', recycle0 = TRUE)


preData = '{
  "uris": [
'
postData ='"
  ],
  "position": 0
}'
actual <- capture.output(cat(preData,uris,postData))
actual <- paste0(actual, collapse = '\n')
urlapi = paste("https://api.spotify.com/v1/playlists/",playlist$id,"/tracks",sep = "")

POST(url = urlapi,config(content_type_json(),token = get_spotify_authorization_code()),body = actual)

#info para crear el request a spotify
#https://stackoverflow.com/questions/62413941/how-to-set-up-post-request-to-add-song-to-spotify-playlist-with-r
