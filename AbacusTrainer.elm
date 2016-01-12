import Html exposing ( div, Html )
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Color exposing (..)
import Random exposing (..)
--import StartApp.Simple exposing( start )

renderGUI : Signal.Address Action  -> Model -> Html.Html
renderGUI picture_address current_state =
    svg
      [ width "2400", height "240", viewBox "0 0 2400 240", fill "lightGray"]
      ( List.concat [ 
        (ten_circles picture_address current_state)
      , 
        [
          g [Svg.Events.onClick ( Signal.message picture_address ( ClickRed ) )]
          [
            ( rect [ x "0", y "120",   width "120", height "120", rx "15", ry "15",  fill "red" ] []  )
           ,( text' [ x "0", y "160", fontSize "55" ] [ text ( toString ( 1 + ( fst current_state.target_number_and_seed ) ) ) ] )
          ]
        ]
      ]
      )

cr30 xpos ypos n labelSize picture_address =
    g [Svg.Events.onClick ( Signal.message picture_address ( ClickedBall n ) )]
      [
        ( circle [ cx xpos, cy ypos, r "30", fill "white", Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"] [] )
      , ( text' [ x xpos , y ypos, fontSize labelSize ] [ text ( toString ( n + 1 ) ) ] )      
      ]

ten_circles : Signal.Address Action -> Model -> List Svg    
ten_circles picture_address current_state =
    let 
      labelSize = Basics.max 0 ( 30 - current_state.score )
      cx  n ball = cr30 ( ball.x |> toString ) ( ball.y |> toString ) n ( labelSize |> toString ) picture_address
    in 
      List.indexedMap cx current_state.ball_locations

justGUI_main = renderGUI picture.address initial_state

live_main: Signal Html
live_main =  Signal.map ( view picture.address ) ( updatep picture.signal )

main = 
    live_main
    --justGUI_main

type alias Position = { x:Int, y:Int}

type alias Model = 
  { 
      user_choice:Int
    , score: Int
    , rng  : Generator Int
    , target_number_and_seed : ( Int, Seed )
    , ball_locations : List Position
  }

resetBallLocation : Int -> Position -> Position
resetBallLocation n ball = { ball| x = ( n * 80 + 600 ) , y = 60 } 

resetPositions : List Position -> List Position
resetPositions current_postions =    
      List.indexedMap resetBallLocation current_postions

shift_ball_left : Position -> Position
shift_ball_left ball = { ball | x = ball.x - 100 }



initial_state: Model
initial_state = { user_choice = 0, score = 0, rng = int 0 9, target_number_and_seed = ( 1,  initialSeed 314 ), ball_locations = resetPositions ( List.repeat 10 {x=0,y=0} ) }

type Action = ClickRed | ClickGreen | Reset | ClickedBall Int

picture : Signal.Mailbox Action 
picture = Signal.mailbox ClickGreen

updatep : Signal Action -> Signal Model
updatep actions  = Signal.foldp update initial_state actions

update : Action -> Model -> Model
update action current_state =
 case action of
  ClickRed   -> 
    { current_state | 
        target_number_and_seed = generate current_state.rng ( snd current_state.target_number_and_seed )
      , user_choice = 0
      , ball_locations = resetPositions current_state.ball_locations 
    }
  ClickGreen -> 
    { current_state | score = current_state.score +1 }
  Reset      -> 
    { current_state | user_choice = 0, ball_locations = resetPositions current_state.ball_locations }
  ClickedBall index ->
      if ( index == ( fst current_state.target_number_and_seed ) ) then 
        { current_state | 
            user_choice = ( index +  1 ) 
          , score = current_state.score + 1
          , ball_locations = ( List.map shift_ball_left ( List.take (index + 1 ) current_state.ball_locations ) ) ++ ( List.drop ( index + 1 ) current_state.ball_locations )
        }
      else
        { current_state | user_choice = ( index +  1 ), score = current_state.score - 1 }


view : Signal.Address Action -> Model -> Html
view address current_state =
  div []
  [
     div [] [ renderGUI picture.address current_state  ]
   , div[ Html.Events.onClick address Reset ] [ Html.text (toString current_state ) ]
  ]
