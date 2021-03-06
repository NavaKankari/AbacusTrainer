import Html exposing (div, li, ol, Html)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Color exposing (..)
import Random exposing (..)
import Window

import Effects exposing (Effects)
import Time exposing (Time, second)

--import Task
--import StartApp.Simple exposing( start )

--MODEL

type alias AnimationState =
    Maybe { prevClockTime : Time
          , elapsedTime   : Time 
          }

type alias Model =
  { window_height     : Int
  , window_width      : Int
  , user_choice       : Int
  , score             : Int
  , rng               : Generator Int
  , target_number_and_seed : (Int, Seed)
  , ball_locations         : List Position
  , correct_locations      : List Position
  , needs_refresh          : Bool
  --, animationState : AnimationState
  }


type alias Position = { x:Int, y:Int }


initial_state: Model
initial_state =
  { window_height = 0
  , window_width  = 0
  , user_choice   = 0
  , score         = 0
  , rng           = int 0 9
  , target_number_and_seed = (0,  initialSeed 314)
  , ball_locations         = resetPositions (List.repeat 10 {x = 0, y = 0})
  , correct_locations      = resetPositions (List.repeat 10 {x = 0, y = 0})
  , needs_refresh          = False
  --, animationState         = Nothing
  }


--UPDATE

type Action = ClickedRefresh | Reset | ClickedBall Int


update : Action -> Model -> Model
update action current_state =
  case action of
    ClickedRefresh ->
      { current_state
        | target_number_and_seed =
            generate current_state.rng (snd current_state.target_number_and_seed)
        , user_choice       = 0
        , ball_locations    = resetPositions current_state.ball_locations
        , correct_locations = resetPositions current_state.ball_locations
        , needs_refresh     = True
      }

    Reset ->
      { current_state
        | user_choice       = 0
        , ball_locations    = resetPositions current_state.ball_locations
        , correct_locations = resetPositions current_state.ball_locations
      }

    ClickedBall index ->
        if ( current_state.needs_refresh == True ) then
          if (index == (fst current_state.target_number_and_seed) ) then
            { current_state
              | user_choice       = (index + 1)
              , score             = current_state.score + 1
              , correct_locations =
                  (List.map shift_ball_left (List.take(index + 1) current_state.ball_locations)) ++ (List.drop (index + 1) current_state.ball_locations)
              , needs_refresh     = False
            }
          else
            { current_state
              | user_choice = (index +  1)
              , score       = current_state.score - 1
            }
        else
           current_state
          


resetBallLocation : Int -> Position -> Position
resetBallLocation n ball =
  { ball | x = (n * 80 + 400), y = 60 }


resetPositions : List Position -> List Position
resetPositions current_postions =
  List.indexedMap resetBallLocation current_postions


shift_ball_left : Position -> Position
shift_ball_left ball =
  { ball | x = ball.x - 100 }


get_target : Model -> Int
get_target current_state =
  (1 + fst current_state.target_number_and_seed)

--VIEW

view : Signal.Address Action -> (Int, Int) -> Model -> Html
view address (w, h) current_state =
  div
    []
    [ div
        []
        [ renderGUI actions.address (w, h) current_state ]
    , div
        []
        [ ol
            []
            [ li
                []
                [ Html.text  "Click in the Green square to get a new target number to click." ]
            , li
                []
                [ Html.text  "Click on the ball with the number shown in the Green square " ]
            , li
                []
                [ Html.text  "The light Blue square indicates your score." ]
            , li
                []
                [ Html.text  "The numbers on the balls become smaller as you get more correct" ]
            , li
                []
                [ Html.text  "And they get bigger when you make mistakes." ]
            ]
        ]
   --, div[ Html.Events.onClick address Reset ] [ Html.text (toString current_state ) ]
    ]


renderGUI : Signal.Address Action  -> (Int, Int) -> Model -> Html.Html
renderGUI address_of_actions_mailbox (w, h) current_state =
    let
      boardWidth  = Basics.min 1200 w |> toString
      boardHeight = Basics.min  240 h |> toString
    in
      svg
        [ width boardWidth
        , height boardHeight
        , viewBox ("0 0 " ++ boardWidth ++ " " ++ boardHeight)
        , fill "lightGray"
        ]
        (List.concat
          [ (ten_circles address_of_actions_mailbox current_state)
          , [ target_area address_of_actions_mailbox (w, h) current_state ]
          , [ score_area  address_of_actions_mailbox (w, h) current_state ]
          ]
        )


target_area : Signal.Address Action  -> (Int, Int) -> Model -> Svg
target_area address_of_actions_mailbox (w, h) current_state =
  g [ 
      Svg.Events.onClick (Signal.message address_of_actions_mailbox ClickedRefresh) 
    ]
    [ rect
        [ x "0"
        , y "120"
        , width "120"
        , height "120"
        , rx "15"
        , ry "15"
        , fill "green"
        ]
        []
    , text'
        [ x "0"
        , y "160"
        , fontSize "55"
        ]
        [ text (toString (get_target current_state ) )]
    ]

score_area : Signal.Address Action  -> (Int, Int) -> Model -> Svg
score_area address_of_actions_mailbox (w, h) current_state =
    g [ ]
    [ rect
        [ x "750"
        , y "120"
        , width "120"
        , height "120"
        , rx "15"
        , ry "15"
        , fill "lightBlue"
        ]
        []
    , text'
        [ x "750"
        , y "160"
        , fontSize "55" ]
        [ text (toString ( current_state.score)) ]
    ]

--cr30 ball_pair n labelSize address_of_actions_mailbox =
--  let 
--    cball = fst ball_pair
--    rball = snd ball_pair
--    rxpos = rball.x |> toString 
--    cxpos = cball.x |> toString
--    ypos  = rball.y |> toString 
--  in
--    g
--      [ Svg.Events.onClick (Signal.message address_of_actions_mailbox (ClickedBall n) )]
--      [ circle
--          [ cx cxpos
--          , cy ypos
--          , r "30"
--          , fill "white"
--          , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
--          ]
--          []
--      , text'
--          [ x cxpos
--          , y ypos
--          , fontSize labelSize
--          ]
--          [ text (toString (n + 1)) ]
--      ]

cr30 ball_pair n labelSize address_of_actions_mailbox =
  let 
    cball = fst ball_pair
    rball = snd ball_pair
    rxpos = rball.x |> toString
    cxpos = cball.x |> toString
    ypos  = rball.y |> toString
    ball_number = (toString (n + 1))
    mpid  = "motion_path_for_ball_" ++ ball_number ++ cxpos
    mp    = if rxpos == cxpos 
            then g [] [] 
            else 
              let 
                dstr = "M" ++ rxpos ++ "," ++ ypos ++ " " ++
                       "L" ++ cxpos ++ "," ++ ypos ++ " " ++
                       "Z" 
              in
                Svg.path
                 [ d dstr
                 , fill "none"
                 , id mpid 
                 ][]
  in 
    g
      [ Svg.Events.onClick (Signal.message address_of_actions_mailbox (ClickedBall n) )]
      [ 
        mp
      , g 
         []
         [
            circle
                [ cx rxpos
                , cy ypos
                , r "30"
                , fill "white"
                , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
                ]  []
          , text'
                [ x rxpos
                , y ypos
                , fontSize labelSize
                ]
                [ text ball_number ]
          ]
      , g
          []
          [
            circle
              [ cx "" -- xpos
              , cy "" -- ypos
              , r "30"
              , fill "white"
              , Svg.Attributes.style "stroke:rgb(0,255,0);stroke-width:2"
              ]
              []
          , text'
              [ x "" -- rxpos
              , y "" -- ypos
              , fontSize labelSize
              ]
              [ text ball_number ]
          , Svg.animateMotion [ dur "2s", repeatCount "1" ] [ Svg.mpath [xlinkHref ( "#" ++ mpid ) ] [] ]
          ]
      ]



ten_circles : Signal.Address Action -> Model -> List Svg
ten_circles address_of_actions_mailbox current_state =
    let
      labelSize      = Basics.max 0 (30 - current_state.score)
      cx n ball_pair = cr30 ball_pair n (labelSize |> toString) address_of_actions_mailbox
      ball_pairs     = List.map2 (,) current_state.correct_locations current_state.ball_locations
    in
      List.indexedMap cx ball_pairs


--SIGNALS

actions : Signal.Mailbox Action
actions = Signal.mailbox Reset


--WIRING

model : Signal Action -> Signal Model
model actions = Signal.foldp update initial_state actions

main: Signal Html
main =  Signal.map2 (view actions.address) Window.dimensions (model actions.signal)


--justGUI_main = renderGUI actions.address ( 1200, 240 ) initial_state


--main =
--    live_main
    --justGUI_main
