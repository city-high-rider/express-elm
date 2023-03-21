module Pages.Home exposing (view)

import Colorscheme exposing (light)
import Element exposing (centerX, column, el, fill, layout, link, none, padding, paragraph, rgb, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html, a, div, h2, p)
import Html.Attributes exposing (href)


view : Html msg
view =
    layout [ Background.color <| light.fg ] <|
        column
            [ width fill ]
            [ row [ centerX ]
                [ el
                    [ Font.color <| light.primary
                    , Font.center
                    , Font.size 35
                    ]
                    (text "Welcome to the website!")
                ]
            , row [ centerX ]
                [ column
                    [ Font.color <| light.bg, centerX ]
                    [ text "This was made as a school project. Feel free to try it out! You can:"
                    , link [ Font.color <| light.secondary, Element.mouseOver [ Font.color light.misc ] ]
                        { url = "/menu"
                        , label = text "Check out the menu"
                        }
                    , link [ Font.color <| light.secondary, Element.mouseOver [ Font.color light.misc ] ]
                        { url = "/login"
                        , label = text "Login to the admin page"
                        }
                    , link [ Font.color <| light.secondary, Element.mouseOver [ Font.color light.misc ] ]
                        { url = "/adminCategories"
                        , label = text "Change the categories"
                        }
                    , link [ Font.color <| light.secondary, Element.mouseOver [ Font.color light.misc ] ]
                        { url = "/adminProducts"
                        , label = text "Change the products"
                        }
                    , paragraph [ Font.color <| light.bg ]
                        [ text "You can always navigate using the header at the top of the page. If you plan on doing admin stuff, you need to log in, otherwise it will not work."
                        ]
                    ]
                ]
            , column [ Font.color light.bg, width fill, padding 25, spacing 20 ]
                [ el [ centerX, Font.color light.primary, Font.size 30 ] (text "Some questions you may have:")
                , paragraph []
                    [ el [ Font.bold ] (text "How come there is no sign up page? ")
                    , text "The example site that we are making at school comes with a sign up page. So how come this one does not? We were never told the full context of the site in class, so I assumed that it was some electronic menu or ordering site being used by a small business. Such a simple website has no need for several accounts in the first place, and furthermore the only reason you might want to have several accounts is if they need different levels of privelege for some reason. I think that it would be more appropriate to have one admin password that just lets you edit the menu. I could add a sign up page and support for several accounts, but I prefer simplicity over needless complexity!"
                    ]
                , paragraph []
                    [ el [ Font.bold ] (text "Why does it looks so different? ")
                    , text "There are two reasons that this site looks different to the example: Firstly, I wrote the whole thing in a completely different language and paradigm, and secondly, I did not style this using CSS, but rather elm-ui. On top of that, styling the website was sort of an afterthought for me. I wanted to get the actual functionality done first, because when I started working on this project in a different language I wanted to see if I could actually make it work before I comitted to using it! I could not afford to waste time making the page look pretty, and it was the last thing on my mind."
                    ]
                , paragraph []
                    [ el [ Font.bold ] (text "Why was this done in Elm? ")
                    , text "I fell in love with the purely functional paradigm a while back, and Elm was sort of my gateway drug to them. It's a language which is desinged for writing front-end code, and I am comfortable using it. Far more so than plain old HTML/CSS or Python/Jinja. I think it's a safer and more elegant way to get things done, and it was far more fun to write than just sitting down and copying the teacher's Python code for hours straight. If you want a more practical reason, Elm is much nicer to do error handling in. It forces you to carefully think about how you model your data before you start writing, and to account for every possible case. What should I display if I can't connect to the database? What if I can't load some resource? What if I did a crap job of modelling my data and now I can't put in text into an object to encode into JSON to be sent as an HTTP request to the server because it's wrapped in a Maybe type so I have to go back and rethink how I want the admin page to work from scratch? All of this checking means that we are guaranteed to get nice input to send back to the server. Doing all this error checking on the front end reliably means we can always send good data to the server, and our backend is more minimal and lightweight. Which is good, because frankly I'm not good at writing Javascript. Also, type theory rocks, and I don't think I can live without a type system anymore."
                    ]
                ]
            ]



{-
   div []
   [ h2 [] [text "Welcome to the website!"]
   , p [] [text "This is a test to try and read and display some information from a database, using an expressjs backend."]
   , a [href "/menu"] [text "go take a look at the menu!"]
   , p [] [text "or"]
   , a [href "/adminCategories"] [text "Manage categories"]
   , p [] []
   , a [href "/adminProducts"] [text "Manage products"]
   ]
-}
