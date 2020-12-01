port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Json exposing (Decoder, field, list, map4, string)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


main : Program (Maybe (List Movie)) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Mvoies App", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : List Movie -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel.favouriteMovies, cmds ]
    )


type alias Model =
    { movies : List Movie
    , searchValue : String
    , favouriteMovies : List Movie
    }


type alias Movie =
    { id : String
    , title : String
    , year : String
    , poster : String
    }


type Msg
    = GotMovies (Result Http.Error (List Movie))
    | ChangeSearchValue String
    | AddFavourites Movie
    | RemoveFavouriteMovie Movie


type MovieItemType
    = MoviesListItem
    | FavouriteListItem


apiKey : String
apiKey =
    "7649c04"


moviesDecoder : Decoder (List Movie)
moviesDecoder =
    map4 Movie
        (field "imdbID" string)
        (field "Title" string)
        (field "Year" string)
        (field "Poster" string)
        |> Json.list
        |> field "Search"


getMovies : String -> Cmd Msg
getMovies searchValue =
    let
        apiUrl =
            "http://www.omdbapi.com/?s=" ++ searchValue ++ "&apikey=" ++ apiKey
    in
    Http.get
        { url = apiUrl
        , expect = Http.expectJson GotMovies moviesDecoder
        }


init : Maybe (List Movie) -> ( Model, Cmd Msg )
init maybeFavouriteMovies =
    ( { movies = []
      , searchValue = ""
      , favouriteMovies = Maybe.withDefault [] maybeFavouriteMovies
      }
    , getMovies ""
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMovies result ->
            case result of
                Ok movies ->
                    ( { model | movies = movies }, Cmd.none )

                Err _ ->
                    ( { model | movies = [] }, Cmd.none )

        ChangeSearchValue value ->
            ( { model | searchValue = value }, getMovies value )

        AddFavourites movie ->
            ( { model | favouriteMovies = List.append [ movie ] model.favouriteMovies }, Cmd.none )

        RemoveFavouriteMovie movie ->
            ( { model | favouriteMovies = List.filter (\favourite -> favourite.id /= movie.id) model.favouriteMovies }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container-fluid movie-app" ]
        [ div [ class "row d-flex align-items-center mt-4 mb-4" ]
            [ viewMovieListHeading "Movies"
            , viewSearchBox model.searchValue
            ]
        , div [ class "row" ]
            (List.map (viewMovieItem MoviesListItem) model.movies)
        , div [ class "row d-flex align-items-center mt-4 mb-4" ]
            [ viewMovieListHeading "Favourites" ]
        , div [ class "row" ]
            (List.map (viewMovieItem FavouriteListItem) model.favouriteMovies)
        ]


viewMovieItem : MovieItemType -> Movie -> Html Msg
viewMovieItem movieType movie =
    case movieType of
        MoviesListItem ->
            div [ class "image-container d-flex justify-content-start m-3" ]
                [ img
                    [ src movie.poster
                    , alt "movie"
                    ]
                    []
                , div
                    [ class "overlay d-flex align-items-center justify-content-center"
                    , onClick (AddFavourites movie)
                    ]
                    [ span [ class "mr-2" ] [ text "Add to Favourites" ]
                    , svg
                        [ SvgAttr.width "1em"
                        , SvgAttr.height "1em"
                        , SvgAttr.viewBox "0 0 16 16"
                        , SvgAttr.class "bi bi-heart-fill"
                        , SvgAttr.fill "red"
                        ]
                        [ path
                            [ SvgAttr.fillRule "evenodd"
                            , SvgAttr.d "M8 1.314C12.438-3.248 23.534 4.735 8 15-7.534 4.736 3.562-3.248 8 1.314z"
                            ]
                            []
                        ]
                    ]
                ]

        FavouriteListItem ->
            div [ class "image-container d-flex justify-content-start m-3" ]
                [ img
                    [ src movie.poster
                    , alt "movie"
                    ]
                    []
                , div
                    [ class "overlay d-flex align-items-center justify-content-center"
                    , onClick (RemoveFavouriteMovie movie)
                    ]
                    [ span [ class "mr-2" ] [ text "Remove from favourites" ]
                    , svg
                        [ SvgAttr.width "1em"
                        , SvgAttr.height "1em"
                        , SvgAttr.viewBox "0 0 16 16"
                        , SvgAttr.class "bi bi-x-square"
                        , SvgAttr.fill "currentColor"
                        ]
                        [ path
                            [ SvgAttr.fillRule "evenodd"
                            , SvgAttr.d "M14 1H2a1 1 0 0 0-1 1v12a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V2a1 1 0 0 0-1-1zM2 0a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2H2z"
                            ]
                            []
                        , path
                            [ SvgAttr.fillRule "evenodd"
                            , SvgAttr.d "M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"
                            ]
                            []
                        ]
                    ]
                ]


viewMovieListHeading : String -> Html Msg
viewMovieListHeading heading =
    div [ class "col" ]
        [ h1 [] [ text heading ]
        ]


viewSearchBox : String -> Html Msg
viewSearchBox sValue =
    div [ class "col col-sm-4" ]
        [ input
            [ class "form-control"
            , value sValue
            , placeholder "Type to search..."
            , onInput ChangeSearchValue
            ]
            []
        ]
