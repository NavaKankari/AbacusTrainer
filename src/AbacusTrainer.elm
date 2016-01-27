module AbacusTrainer (Model, Action, init, effective_update, view) where

import Html exposing (div, li, ol, Html)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Color exposing (..)
import Random exposing (..)
import Window

import Easing exposing (ease, easeOutBounce, float)
import Effects exposing (Effects)
import Time exposing (Time, second)
--import Task
--import StartApp.Simple exposing( start )

--MODEL


type alias AnimationState =
    Maybe { prevClockTime : Time, elapsedTime : Time }

type alias Model =
  { window_height: Int
  , window_width: Int
  , user_choice: Int
  , score: Int
  , rng  : Generator Int
  , target_number_and_seed : (Int, Seed)
  , ball_locations : List Position
  , needs_refresh : Bool
  , angle         : Float
  , x_offset      : Int
  , animationState : AnimationState
  }


type alias Position = { x:Int, y:Int }


initial_state: Model
initial_state =
  { window_height          = 0
  , window_width           = 0
  , user_choice            = 0
  , score                  = 0
  , rng                    = int 0 9
  , target_number_and_seed = (1,  initialSeed 314)
  , ball_locations         = resetPositions (List.repeat 10 {x = 0, y = 0})
  , needs_refresh          = False
  , angle                  = 0
  , x_offset               = 0
  , animationState         = Nothing
  }


init : ( Model, Effects Action)
init =
  ( initial_state
  , Effects.none
  )



rotateStep = 360
duration = second

--UPDATE

type Action = ClickedRefresh | Reset | ClickedBall Int | Tick Time


update : Action -> Model -> Model
update action current_state =
  case action of
    ClickedRefresh ->
      { current_state
        | target_number_and_seed =
            generate current_state.rng (snd current_state.target_number_and_seed)
        , user_choice = 0
        , ball_locations = resetPositions current_state.ball_locations
        , needs_refresh = True
      }

    Reset ->
      { current_state
        | user_choice = 0
        , ball_locations = resetPositions current_state.ball_locations
      }

    ClickedBall index ->
        if ( current_state.needs_refresh == True ) then
          if (index == (fst current_state.target_number_and_seed) ) then
            { current_state
              | user_choice = (index + 1)
              , score = current_state.score + 1
              , ball_locations =
                  (List.map shift_ball_left (List.take(index + 1) current_state.ball_locations)) ++ (List.drop (index + 1) current_state.ball_locations)
              , needs_refresh = False
            }
          else
            { current_state
              | user_choice = (index +  1)
              , score = current_state.score - 1
            }
        else
           current_state
    Tick _ ->
      current_state
          
effective_update : Action -> Model -> (Model, Effects Action)
effective_update action current_state =
  case action of
    ClickedRefresh ->
      case current_state.animationState of
        Nothing ->
          ( current_state, Effects.tick Tick )

        Just _ ->
          ( update action current_state, Effects.none )

    Reset ->
      case current_state.animationState of
        Nothing ->
          ( current_state, Effects.tick Tick )

        Just _ ->
          ( update action current_state, Effects.none )

    ClickedBall index ->
      case current_state.animationState of
        Nothing ->
          ( current_state, Effects.tick Tick )

        Just _ ->
          ( update action current_state, Effects.none )
          

    Tick clockTime ->
      let
        newElapsedTime =
          case current_state.animationState of
            Nothing ->
              0

            Just {elapsedTime, prevClockTime} ->
              elapsedTime + (clockTime - prevClockTime)
      in
        if newElapsedTime > duration then
          ( 
            { current_state
              | angle          = current_state.angle + rotateStep
              , x_offset       = current_state.x_offset + rotateStep
              , animationState = Nothing  
            }
            , Effects.none
          )
        else
          ( 
            { current_state
              | angle          = current_state.angle
              , x_offset       = current_state.x_offset
              , animationState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
            }
            , Effects.tick Tick
          )


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

toOffset : AnimationState -> Float
toOffset animationState =
  case animationState of
    Nothing ->
      0

    Just {elapsedTime} ->
      ease easeOutBounce Easing.float 0 rotateStep duration elapsedTime


view : Signal.Address Action -> (Int, Int) -> Model -> Html
view address_of_actions_mailbox (w, h) current_state =
  div
    []
    [ div
        []
        [ renderGUI address_of_actions_mailbox (w, h) current_state ]
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
      boardHeight = Basics.min 240 h |> toString
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
  let
    angle    = current_state.angle    + toOffset current_state.animationState
    x_offset = ( 
                  toFloat current_state.x_offset 
                + 20
                --+ toOffset current_state.animationState 
                --- 100 
               ) |> round
  in
    g [ transform ("rotate(" ++ toString angle ++ ")")
      , Svg.Events.onClick (Signal.message address_of_actions_mailbox ClickedRefresh) 
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


cr30 xpos ypos n labelSize address_of_actions_mailbox =
    g
      [ Svg.Events.onClick (Signal.message address_of_actions_mailbox (ClickedBall n) )]
      [ circle
          [ cx xpos
          , cy ypos
          , r "30"
          , fill "white"
          , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
          ]
          []
      , text'
          [ x xpos
          , y ypos
          , fontSize labelSize
          ]
          [ text (toString (n + 1)) ]
      ]


ten_circles : Signal.Address Action -> Model -> List Svg
ten_circles address_of_actions_mailbox current_state =
    let
      labelSize = Basics.max 0 (30 - current_state.score)
      cx n ball = cr30 (ball.x |> toString) (ball.y |> toString) n (labelSize |> toString) address_of_actions_mailbox
    in
      List.indexedMap cx current_state.ball_locations


--SIGNALS

--actions : Signal.Mailbox Action
--actions = Signal.mailbox Reset


--WIRING

--model : Signal Action -> Signal ( Model, Effects Action )
--model actions = Signal.foldp effective_update initial_state actions

--main: Signal Html
--main =  Signal.map2 (view actions.address) Window.dimensions (model actions.signal)


--justGUI_main = renderGUI actions.address ( 1200, 240 ) initial_state


--main =
--    live_main
    --justGUI_main
