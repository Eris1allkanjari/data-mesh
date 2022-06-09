module View.Discover exposing (deleteConfirmationDialog, view)

import Dialog.Common as Dialog
import GenericDict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, target, type_, value)
import Html.Events exposing (onClick)
import Json.Extras as Json
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Table exposing (defaultCustomizations)
import Table.Extras as Table
import Types exposing (..)
import UIKit
import Url exposing (Url)
import View.Common exposing (..)
import View.Icons exposing (Icon(..), icon)
import View.Tooltips exposing (tooltip)


view : Maybe QualifiedName -> Model -> Html Msg
view activeStreamKey model =
    webDataView
        (\streams ->
            let
                activeStream =
                    activeStreamKey
                        |> Maybe.map
                            (\key -> Dict.get unQualifiedName key streams)
                        |> Maybe.withDefault Nothing
            in
            div [ class "discover-pane" ]
                [ header []
                    [ Markdown.toHtml [] discoveryIntro ]
                , div [ class "discover-main" ]
                    [ h4 []
                        [ text
                            (case model.actuatorInfo of
                                Success info ->
                                    "Produktet e te dhenave per Domain Analytics"

                                _ ->
                                    "Produktet e te dhenave"
                            )
                        , tooltip "Zbulo produktet e te dhenave qe jane publike per domain tuaj"
                        ]
                    , Table.view
                        (tableConfig activeStreamKey)
                        model.dataProductsTableState
                        (streams
                            |> Dict.values
                            |> filterDataProducts
                        )
                    ]
                , div [ class "discover-detail" ]
                    [ h4 []
                        [ text "Detajet e produktit te dhenave"
                        , tooltip "Permban te gjithe te dhenat e nevojshme per produktine te dhenave."
                        ]
                    , streamDetailView
                        (RemoteData.toMaybe model.actuatorInfo)
                        activeStream
                    ]
                , footer []
                    [ Markdown.toHtml [] discoveryOutro ]
                ]
        )
        model.streams


discoveryIntro : String
discoveryIntro =
    """
### Zbulo produkte te dhenash te cilat mund ti konsumosh

*Roli: Konsumator i mundshem*

Ky tab mundeson te eksplorojme produktet e te dhenave.Produktet e te Dhenave mund te publikohen nga domain te ndryshem, dhe gjetja e atyre qe ju duhen eshte e nevojshme per ndertimin e nje aplikacioni te ri.
Ne kete tab mund te :
  - Eksplorojme produktet e te dhenave
  - Shikojme pershkrime,skema si dhe detaje te tjera
  - Identifikojme produktet e te dhenave te nevojshme per aplikacionin tone

"""


discoveryOutro : String
discoveryOutro =
    """

    """


filterDataProducts : List Stream -> List DataProduct
filterDataProducts =
    List.filterMap
        (\stream ->
            case stream of
                StreamDataProduct dataProduct ->
                    Just dataProduct

                StreamTopic _ ->
                    Nothing
        )


tableConfig : Maybe QualifiedName -> Table.Config DataProduct Msg
tableConfig activeStreamKey =
    Table.customConfig
        { toId = .qualifiedName >> unQualifiedName
        , toMsg = SetDataProductsTableState
        , columns =
            [ Table.stringColumn "Emri" .name
            , Table.stringColumnWithAttributes
                "Pershkrimi"
                [ class "description" ]
                .description
            , Table.stringColumn "Pronari" .owner
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs =
                    [ UIKit.table
                    , UIKit.tableDivider
                    , UIKit.tableStriped
                    , UIKit.tableSmall
                    ]
                , rowAttrs =
                    \dataProduct ->
                        let
                            isActive =
                                Just dataProduct.qualifiedName == activeStreamKey
                        in
                        (if isActive then
                            [ UIKit.active ]

                         else
                            []
                        )
                            ++ [ selectable
                               , onClick
                                    (ChangeView
                                        (Discover
                                            (if isActive then
                                                Nothing

                                             else
                                                Just dataProduct.qualifiedName
                                            )
                                        )
                                    )
                               ]
            }
        }


disabledForm : List (Html msg) -> Html msg
disabledForm children =
    form [ UIKit.formHorizontal ]
        [ fieldset
            [ UIKit.fieldset
            , disabled True
            ]
            children
        ]


streamDetailView : Maybe ActuatorInfo -> Maybe Stream -> Html Msg
streamDetailView mActuatorInfo mStream =
    case mStream of
        Nothing ->
            i [] [ text "Zgjidh nje produkt nga tabela ne te majte." ]

        Just (StreamDataProduct dataProduct) ->
            div []
                [ table
                    [ UIKit.table
                    , UIKit.tableDivider
                    , class "table-horizontal"
                    ]
                    (List.map
                        (\( title, content ) ->
                            tr []
                                [ th [] [ text title ]
                                , td [] [ content ]
                                ]
                        )
                        [ ( "Emri",  text dataProduct.name )
                        , ( "Domain",  text (unDomain dataProduct.domain) )
                        , ( "Pronari",  text dataProduct.owner )
                        , ( "Kualiteti", text (showProductQuality dataProduct.quality) )
                        , ( "SLA", text (showProductSla dataProduct.sla) )
                        , ( "Schema"
                          , pre []
                                [ code []
                                    [ text (Json.prettyPrintIfPossible dataProduct.schema.schema)
                                    ]
                                ]
                          )
                        ]
                    )
                , case mActuatorInfo of
                    Just actuatorInfo ->
                        div [ UIKit.margin, UIKit.width_1_1 ]
                            (List.intersperse (text " ")
                                (List.map (linkButton actuatorInfo.hostedMode)
                                    [ ( "Detaje te topic", dataProduct.urls.portUrl, TopicScreenshot )
                                    , ( "Prejardhja e te dhenave", dataProduct.urls.lineageUrl, LineageScreenshot )
                                    , ( "Eksporto", dataProduct.urls.exportUrl, ExportScreenshot )
                                    ]
                                )
                            )

                    Nothing ->
                        span [] []
                ]

        Just (StreamTopic topic) ->
            disabledForm
                [ stringInput "Name" topic.name
                ]


linkButton : HostedMode -> ( String, Url, ScreenshotTarget ) -> Html Msg
linkButton hostedMode ( description, url, screenshotTarget ) =
    let
        sharedAttributes =
            [ UIKit.button
            , UIKit.buttonPrimary
            , UIKit.buttonSmall
            , href (Url.toString url)
            , target "_blank"
            ]
    in
    case hostedMode of
        Hosted ->
            button
                (sharedAttributes
                    ++ [ onClick (ShowScreenshot screenshotTarget) ]
                )
                [ text description ]

        Local ->
            a
                (sharedAttributes
                    ++ [ href (Url.toString url)
                       , target "_blank"
                       ]
                )
                [ text description
                , icon ExternalLink
                ]


stringInput : String -> String -> Html msg
stringInput inputLabel inputValue =
    div []
        [ label [ UIKit.formLabel ] [ text inputLabel ]
        , div [ UIKit.formControls ]
            [ input
                [ type_ "text"
                , UIKit.input
                , value inputValue
                ]
                []
            ]
        ]


deleteConfirmationDialog : DataProduct -> Dialog.Config Msg
deleteConfirmationDialog dataProduct =
    { closeMessage = Just AbandonDeleteDataProduct
    , containerClass = Nothing
    , header =
        Just
            (h3 [ UIKit.modalTitle ]
                [ text "Are you sure?" ]
            )
    , body =
        Just
            (div []
                [ p []
                    [ text "Are you sure you want to remove "
                    , code [] [ text dataProduct.name ]
                    , text " from the data mesh?"
                    ]
                , p []
                    [ text "Ensure that all consumers have been informed of the removal and have migrated accordingly." ]
                ]
            )
    , footer =
        Just
            (div []
                [ button
                    [ UIKit.button
                    , UIKit.buttonDefault
                    , onClick AbandonDeleteDataProduct
                    ]
                    [ text "Cancel" ]
                , button
                    [ UIKit.button
                    , UIKit.buttonDanger
                    , onClick (ConfirmDeleteDataProduct dataProduct.qualifiedName)
                    ]
                    [ text "Confirm" ]
                ]
            )
    }
