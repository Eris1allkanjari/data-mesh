module View.Manage exposing (publishDialog, view)

import Dialog.Common as Dialog
import GenericDict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (autofocus, checked, class, disabled, for, id, name, placeholder, rows, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Markdown
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData(..), WebData)
import Result.Extras as Result
import Set exposing (Set)
import Table exposing (defaultCustomizations)
import Table.Extras as Table
import Tuple exposing (pair)
import Types exposing (..)
import UIKit
import Validate exposing (validate)
import View.Common exposing (..)
import View.Tooltips exposing (tooltip)


view : Model -> Html Msg
view model =
    div [ class "manage-pane" ]
        [ header []
            [ Markdown.toHtml [] manageIntro ]
        , div [ class "manage-main" ]
            [ h4 []
                [ text "Kafka Topics"
                , p []
                    [ small []
                        [ text "Te publikueshme si produkt te dhenash"
                        , tooltip "Keto event streams bejne pjese ne domain, dhe mund te publikohen si event streams.Ky veprim do ekspozoje keta event streams ndaj te gjithe organizates tuaj."
                        ]
                    ]
                ]
            , case model.deleteResult of
                Failure err ->
                    errorView err

                _ ->
                    span [] []
            , webDataView
                (splitStreamTablesView model.dataProductsTableState)
                (RemoteData.map2 pair model.streams model.actuatorInfo)
            ]
        , footer []
            [ Markdown.toHtml [] manageOutro ]
        ]


splitStreamTablesView : Table.State -> ( Dict QualifiedName Stream, ActuatorInfo ) -> Html Msg
splitStreamTablesView dataProductsTableState ( streams, actuatorInfo ) =
    let
        ( ourStreams, otherStreams ) =
            List.partition
                (\stream ->
                    case getStreamDomain stream of
                        Nothing ->
                            True

                        Just domain ->
                            domain == actuatorInfo.domain
                )
                (Dict.values streams)

        showTableUnlessEmpty tableConfigFlags items =
            Table.view
                (tableConfig tableConfigFlags)
                dataProductsTableState
                items
    in
    div []
        [ showTableUnlessEmpty
            { showControls = True
            , caption = Nothing
            }
            ourStreams
        , h4 [] [ text "Produkte te Dhenash nga domain te tjere", tooltip "Keta jane produktet e te dhenave qe jane publikuar nga domain ose skuadra te tjera.Duke qene se nuk kemi te drejtat e nevojshme, nuk mund ti modifikojme ata." ]
        , showTableUnlessEmpty
            { showControls = False
            , caption = Nothing
            }
            otherStreams
        ]


manageIntro : String
manageIntro =
    """
### Publiko Produktet e te Dhenave ne Data Mesh

*Roli: Pronari i Produktit te Dhenave*

Ky tab lejon te menaxhojme produktet e te dhenave ne domain.Kemi dy tabela kryesore:
- Kafka Topics te domain, te cilat mund te publikohen si Produkte te Dhenash
- Produktet e te Dhenave te ofruara nga cdo domain
- Kjo faqje tregon vetem produktet e te dhenave te bazuara ne Kafka Topics. Nje implementim me i thelluar duhet te lejonte dhe produkte te dhenash nga API te tjera.
"""


manageOutro : String
manageOutro =
    """
    """


type alias TableConfigFlags msg =
    { showControls : Bool
    , caption : Maybe (List (Html msg))
    }


tableConfig : TableConfigFlags Msg -> Table.Config Stream Msg
tableConfig { showControls, caption } =
    Table.customConfig
        { toId = streamQualifiedName >> unQualifiedName
        , toMsg = SetDataProductsTableState
        , columns =
            [ Table.stringColumnWithAttributes
                "Emri"
                [ UIKit.width_1_10 ]
                getStreamName
            , Table.stringColumnWithAttributes
                "Domain"
                [ UIKit.width_1_10 ]
                (getStreamDomain >> Maybe.map unDomain >> withDefault "-")
            , Table.stringColumnWithAttributes
                "Pershkrimi"
                [ UIKit.width_2_10 ]
                (getStreamDescription >> withDefault "-")
            , Table.stringColumnWithAttributes
                "Pronari"
                [ UIKit.width_1_10 ]
                (getStreamOwner >> withDefault "-")
            , Table.stringColumnWithAttributes
                "Kualiteti"
                [ UIKit.width_1_10 ]
                (getStreamQuality >> maybe "-" showProductQuality)
            , Table.stringColumnWithAttributes
                "SLA"
                [ UIKit.width_1_10 ]
                (getStreamSLA >> maybe "-" showProductSla)
            , Table.veryCustomColumn
                { name = "Veprimi"
                , viewData =
                    \dataProduct ->
                        Table.HtmlDetails [ UIKit.width_2_10 ]
                            (if showControls then
                                [ publishButton dataProduct ]

                             else
                                [ span [] [] ]
                            )
                , sorter =
                    Table.unsortable
                }
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs =
                    [ UIKit.table
                    , UIKit.tableDivider
                    , UIKit.tableStriped
                    , UIKit.tableSmall
                    ]
                , caption =
                    case caption of
                        Nothing ->
                            Nothing

                        Just contents ->
                            Just
                                (Table.HtmlDetails []
                                    contents
                                )
                , thead = Table.infoThead columnTooltips
            }
        }


columnTooltips : String -> Maybe String
columnTooltips name =
    case name of
        "Emri" ->
            Just "Mund te jete emer i thjeshte ose dhe nje URI e plote"

        "Domain" ->
            Just "Domain te cilit i perkete produkti i te dhenave"

        "Description" ->
            Just "Pershkrim i thjeshte se per cfare sherben ky produkt te dhenash."

        "Owner" ->
            Just "Skuadra qe ka si detyre prodhimin , evoluimin dhe mirembajtjen e kualitetit dhe SLA."

        "Quality" ->
            Just "Tregon nivelin e gatishmerise per production"

        "SLA" ->
            Just "Niveli minimum i sherbimeve te pritura ne rast te gabimi"

        "Action" ->
            Just "Te lejon te publikosh ose heqesh produkte te dhenash nga Data Mesh"

        _ ->
            Nothing


publishButton : Stream -> Html Msg
publishButton stream =
    case
        stream
    of
        StreamDataProduct dataProduct ->
            button
                [ UIKit.button
                , UIKit.width_1_1
                , UIKit.buttonDanger
                , onClick (DeleteDataProduct dataProduct.qualifiedName)
                ]
                [ text "Fshij nga Data Mesh" ]

        StreamTopic topic ->
            button
                [ UIKit.button
                , UIKit.width_1_1
                , UIKit.buttonPrimary
                , onClick (StartPublishDialog topic.qualifiedName)
                ]
                [ text "Shto ne Data Mesh" ]


publishDialog : WebData PublishFormResult -> PublishForm -> Dialog.Config Msg
publishDialog result model =
    let
        validationResult =
            validate publishFormValidator model

        hasValidationError err =
            case validationResult of
                Ok _ ->
                    False

                Err errs ->
                    List.member err errs
    in
    { closeMessage = Just AbandonPublishDialog
    , containerClass = Nothing
    , header =
        Just
            (div [ UIKit.modalTitle ]
                [ text ("Publiko: " ++ model.topic.name) ]
            )
    , body =
        Just
            (div []
                [ p []
                    [ text "Vendos taget e nevojshem per Produktin e te Dhenave." ]
                , case result of
                    Failure err ->
                        errorView err

                    Success _ ->
                        text ""

                    Loading ->
                        text ""

                    NotAsked ->
                        text ""
                , form [ UIKit.formHorizontal ]
                    [ fieldset
                        [ UIKit.fieldset
                        , disabled (RemoteData.isLoading result)
                        ]
                        [ div []
                            [ label [ UIKit.formLabel ] [ text "Pronari" ]
                            , div [ UIKit.formControls ]
                                [ select
                                    ([ onInput (PublishFormMsg << PublishFormSetOwner)
                                     , value model.owner
                                     , UIKit.select
                                     ]
                                        ++ (if hasValidationError OwnerInvalid then
                                                [ UIKit.formDanger ]

                                            else
                                                []
                                           )
                                    )
                                    (validOwners
                                        |> Set.insert model.owner
                                        |> Set.toList
                                        |> List.map (\owner -> option [ value owner ] [ text owner ])
                                    )
                                ]
                            ]
                        , div []
                            [ label [ UIKit.formLabel ] [ text "Pershkrimi" ]
                            , div [ UIKit.formControls ]
                                [ textarea
                                    [ UIKit.textarea
                                    , value model.description
                                    , rows 2
                                    , disabled True
                                    , onInput (PublishFormMsg << PublishFormSetDescription)
                                    ]
                                    []
                                ]
                            ]
                        , radioButtonGroup
                            "Kualiteti"
                            (PublishFormMsg << PublishFormSetQuality)
                            showProductQuality
                            (Just model.quality)
                            allProductQualities
                        , radioButtonGroup
                            "SLA"
                            (PublishFormMsg << PublishFormSetSla)
                            showProductSla
                            (Just model.sla)
                            allProductSlas
                        , hr [] []
                        , div [ UIKit.formControlsText ]
                            [ input
                                ([ type_ "checkbox"
                                 , id "terms_acknowledged"
                                 , UIKit.checkbox
                                 , checked model.termsAcknowledged
                                 , onCheck (PublishFormMsg << PublishFormSetTermsAcknowledged)
                                 ]
                                    ++ (if hasValidationError TermsNotAcknowledged then
                                            [ UIKit.formDanger ]

                                        else
                                            []
                                       )
                                )
                                []
                            , label
                                [ for "terms_acknowledged"
                                ]
                                [ text " "
                                , text "Pranoj qe duke publikouar kete Produkt te Dhenash,jam dakord me pikat e treguara ne SLA."
                                ]
                            ]
                        ]
                    ]
                , case validationResult of
                    Ok _ ->
                        span [] []

                    Err validationErrors ->
                        div [ UIKit.alert, UIKit.alertDanger ]
                            (List.map formatValidationError validationErrors)
                ]
            )
    , footer =
        Just
            (div []
                [ button
                    [ UIKit.button
                    , UIKit.buttonDefault
                    , UIKit.modalClose
                    , disabled (RemoteData.isLoading result)
                    , onClick AbandonPublishDialog
                    ]
                    [ text "Anullo" ]
                , button
                    [ UIKit.button
                    , UIKit.buttonPrimary
                    , disabled (RemoteData.isLoading result || Result.isErr validationResult)
                    , onClick (PublishDataProduct model)
                    ]
                    [ text "Publiko" ]
                ]
            )
    }


radioButtonGroup : String -> (a -> msg) -> (a -> String) -> Maybe a -> List a -> Html msg
radioButtonGroup radioName handler toStr activeRadioValue radioValues =
    div []
        [ div [ UIKit.formLabel ] [ text radioName ]
        , div [ UIKit.formControls, UIKit.formControlsText ]
            (radioValues
                |> List.map
                    (radioButtonInput
                        radioName
                        handler
                        toStr
                        activeRadioValue
                    )
                |> List.intersperse (text nbsp)
            )
        ]


radioButtonInput : String -> (a -> msg) -> (a -> String) -> Maybe a -> a -> Html msg
radioButtonInput radioName handler toStr activeRadioValue radioValue =
    label []
        [ input
            [ type_ "radio"
            , name radioName
            , UIKit.radio
            , value (toStr radioValue)
            , onInput (always (handler radioValue))
            , checked (activeRadioValue == Just radioValue)
            ]
            []
        , text nbsp
        , text (toStr radioValue)
        ]


getStreamName : Stream -> String
getStreamName stream =
    case stream of
        StreamDataProduct dataProduct ->
            dataProduct.name

        StreamTopic topic ->
            topic.name


getStreamDomain : Stream -> Maybe Domain
getStreamDomain =
    getDataProduct >> Maybe.map .domain


getStreamDescription : Stream -> Maybe String
getStreamDescription =
    getDataProduct >> Maybe.map .description


getStreamOwner : Stream -> Maybe String
getStreamOwner =
    getDataProduct >> Maybe.map .owner


getStreamQuality : Stream -> Maybe ProductQuality
getStreamQuality =
    getDataProduct >> Maybe.map .quality


getStreamSLA : Stream -> Maybe ProductSla
getStreamSLA =
    getDataProduct >> Maybe.map .sla


maybe : b -> (a -> b) -> Maybe a -> b
maybe default fn =
    Maybe.map fn >> Maybe.withDefault default


getDataProduct : Stream -> Maybe DataProduct
getDataProduct stream =
    case stream of
        StreamDataProduct dataProduct ->
            Just dataProduct

        StreamTopic _ ->
            Nothing


formatValidationError : PublishFormError -> Html msg
formatValidationError error =
    div []
        [ case error of
            OwnerInvalid ->
                text "Please select an owner for this product."

            TermsNotAcknowledged ->
                text "Please acknowledge the SLA requirements."
        ]
