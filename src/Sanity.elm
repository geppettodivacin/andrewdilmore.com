module Sanity exposing (fetchCollections)

import Aliases exposing (Album, Collection)
import Http
import Json.Decode as Decode
import Url.Builder as Builder


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


fetchCollections : (Result Http.Error (List Collection) -> msg) -> Cmd msg
fetchCollections toMsg =
    let
        query =
            """
            *[_type == 'collection']
            { title,
              slug,
              'albums': albums[]->{'thumbnailUrl': thumbnail.asset->url, slug, title}
            }
            """
    in
    Http.get
        { url = sanityQueryUrl query
        , expect = Http.expectJson toMsg (resultDecoder (Decode.list collectionDecoder))
        }
