module Sanity exposing (fetchCollections, fetchFullSizeAlbum)

import Aliases exposing (Album, Collection, FullSizeImage)
import Http
import Json.Decode as Decode
import String.Interpolate exposing (interpolate)
import Url.Builder as Builder


fetchCollections : (Result Http.Error (List Collection) -> msg) -> Cmd msg
fetchCollections toMsg =
    let
        query =
            """
            *[_type == 'collection']
            {
              title,
              slug,
              'albums': albums[]->{'thumbnailUrl': thumbnail.asset->url, slug, title}
            }
            """

        addImageResolutionsToAlbum : Album -> Album
        addImageResolutionsToAlbum album =
            { album | thumbnailUrl = Builder.crossOrigin album.thumbnailUrl [] [Builder.int "h" 400, Builder.int "w" 400]}

        addImageResolutionsToCollections : List Collection -> List Collection
        addImageResolutionsToCollections collections =
            collections
                |> List.map (\collection -> { collection | albums = List.map addImageResolutionsToAlbum collection.albums})

        addImageResolutionsToResult : Result Http.Error (List Collection) -> Result Http.Error (List Collection)
        addImageResolutionsToResult result =
            result
                |> Result.map addImageResolutionsToCollections
    in
    Http.get
        { url = sanityQueryUrl query
        , expect = Http.expectJson (toMsg << addImageResolutionsToResult) (resultDecoder (Decode.list collectionDecoder))
        }


fetchFullSizeAlbum : (Result Http.Error (List FullSizeImage) -> msg) -> String -> Cmd msg
fetchFullSizeAlbum toMsg albumSlug =
    let
        query =
            """
            *[_type == 'album' && slug.current == '{0}'].contents[]
            {
              'url': asset->url
            }
            """
            |> (\q -> interpolate q [ albumSlug ])
    in
    Http.get
        { url = sanityQueryUrl query
        , expect = Http.expectJson toMsg (resultDecoder (Decode.list fullSizeImageDecoder))
        }


sanityQueryUrl : String -> String
sanityQueryUrl groqQuery =
    Builder.crossOrigin
        "https://khv1ba20.api.sanity.io"
        [ "v2021-10-21", "data", "query", "production" ]
        [ Builder.string "query" groqQuery ]


resultDecoder : Decode.Decoder innerType -> Decode.Decoder innerType
resultDecoder innerDecoder =
    Decode.field "result" innerDecoder


albumDecoder : Decode.Decoder Album
albumDecoder =
    let
        titleDecoder =
            Decode.field "title" Decode.string

        slugDecoder =
            Decode.at [ "slug", "current" ] Decode.string

        thumbnailUrlDecoder =
            Decode.field "thumbnailUrl" Decode.string
    in
    Decode.map3
        (\title slug thumbnailUrl -> { title = title, slug = slug, thumbnailUrl = thumbnailUrl })
        titleDecoder
        slugDecoder
        thumbnailUrlDecoder


collectionDecoder : Decode.Decoder Collection
collectionDecoder =
    let
        titleDecoder =
            Decode.field "title" Decode.string

        slugDecoder =
            Decode.at [ "slug", "current" ] Decode.string

        albumsDecoder =
            Decode.field "albums" (Decode.list albumDecoder)
    in
    Decode.map3
        (\title slug albums -> { title = title, slug = slug, albums = albums, heightAdjustment = 50 })
        titleDecoder
        slugDecoder
        albumsDecoder


fullSizeImageDecoder : Decode.Decoder FullSizeImage
fullSizeImageDecoder =
    let
        urlDecoder =
            Decode.field "url" Decode.string
    in
    Decode.map
        (\url -> { url = url })
        urlDecoder
