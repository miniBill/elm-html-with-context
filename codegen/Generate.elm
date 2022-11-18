module Generate exposing (main)

import Elm
import Elm.Annotation as Type
import Elm.Parser
import Elm.Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Gen.CodeGen.Generate as Generate
import Gen.Debug
import Gen.Html
import Gen.Html.Attributes
import Gen.Html.Keyed
import Gen.Html.WithContext.Internal
import Gen.List
import Maybe.Extra
import Result.Extra
import Set exposing (Set)


main : Program String () ()
main =
    Generate.fromText toFiles


toFiles : String -> List Elm.File
toFiles helperFiles =
    case
        helperFiles
            |> String.split "\nmodule "
            |> Result.Extra.combineMap parseHelper
    of
        Err err ->
            [ Elm.file [ "Html" ]
                [ Gen.Debug.todo "failed"
                    |> Elm.declaration "failed"
                    |> Elm.withDocumentation err
                ]
            ]

        Ok rawHelpers ->
            let
                context : Elm.Processing.ProcessContext
                context =
                    List.foldl Elm.Processing.addFile Elm.Processing.init rawHelpers
            in
            List.map
                (\rawHelper ->
                    let
                        helper =
                            Elm.Processing.process context rawHelper
                    in
                    helperToFile helper
                )
                rawHelpers


helperToFile : File.File -> Elm.File
helperToFile file =
    let
        ( originalModuleName, exposing_ ) =
            case Node.value file.moduleDefinition of
                Module.NormalModule data ->
                    ( Node.value data.moduleName, Node.value data.exposingList )

                Module.PortModule data ->
                    ( Node.value data.moduleName, Node.value data.exposingList )

                Module.EffectModule data ->
                    ( Node.value data.moduleName, Node.value data.exposingList )

        exposingSet =
            case exposing_ of
                Exposing.All _ ->
                    -- The html package doesn't do this, it's fine
                    Set.empty

                Exposing.Explicit exposed ->
                    exposed
                        |> List.map Node.value
                        |> List.filterMap
                            (\v ->
                                case v of
                                    Exposing.FunctionExpose name ->
                                        Just name

                                    Exposing.InfixExpose _ ->
                                        Nothing

                                    Exposing.TypeOrAliasExpose name ->
                                        Just name

                                    Exposing.TypeExpose { name } ->
                                        Just name
                            )
                        |> Set.fromList

        moduleName : List String
        moduleName =
            [ "Html", "WithContext" ] ++ List.drop 1 originalModuleName

        declarations : List Elm.Declaration
        declarations =
            List.filterMap
                (\declaration ->
                    Node.value declaration
                        |> convertDeclaration exposingSet moduleName
                )
                file.declarations
    in
    Elm.fileWith moduleName
        { docs = List.map Elm.docs
        , aliases =
            [ ( [ "Html", "WithContext", "Internal" ], "Internal" )
            , ( [ "Json", "Encode" ], "Json" )
            , ( [ "Json", "Decode" ], "Json" )
            ]
        }
        (customDeclarations moduleName ++ declarations)


customDeclarations : List String -> List Elm.Declaration
customDeclarations moduleName =
    case moduleName of
        [ "Html", "WithContext" ] ->
            []

        _ ->
            []


convertDeclaration :
    Set String
    -> List String
    -> Declaration.Declaration
    -> Maybe Elm.Declaration
convertDeclaration exposingSet moduleName declaration =
    let
        ( value, doc ) =
            case declaration of
                Declaration.FunctionDeclaration { documentation, signature } ->
                    ( Maybe.map Node.value signature
                        |> Maybe.andThen
                            (\sign ->
                                if Set.member (Node.value sign.name) exposingSet then
                                    convertFunction moduleName sign

                                else
                                    Nothing
                            )
                    , Maybe.map Node.value documentation
                    )

                Declaration.AliasDeclaration { documentation, name } ->
                    ( if Set.member (Node.value name) exposingSet then
                        convertAlias <| Node.value name

                      else
                        Nothing
                    , Maybe.map Node.value documentation
                    )

                Declaration.CustomTypeDeclaration _ ->
                    ( Nothing, Nothing )

                Declaration.PortDeclaration _ ->
                    ( Nothing, Nothing )

                Declaration.InfixDeclaration _ ->
                    ( Nothing, Nothing )

                Declaration.Destructuring _ _ ->
                    ( Nothing, Nothing )
    in
    Maybe.map
        (\v ->
            v
                |> Elm.expose
                |> (case doc of
                        Nothing ->
                            identity

                        Just d ->
                            Elm.withDocumentation (String.slice 3 -3 d)
                   )
        )
        value


convertAlias : String -> Maybe Elm.Declaration
convertAlias name =
    case name of
        "Html" ->
            Just <|
                Elm.alias "Html" <|
                    Gen.Html.WithContext.Internal.annotation_.html
                        (Type.var "context")
                        (Type.var "msg")

        "Attribute" ->
            Just <|
                Elm.alias "Attribute" <|
                    Gen.Html.WithContext.Internal.annotation_.attribute
                        (Type.var "context")
                        (Type.var "msg")

        _ ->
            Just <|
                Elm.declaration "fail" <|
                    Gen.Debug.todo ("no idea how to convert alias " ++ name)


convertFunction : List String -> Signature -> Maybe Elm.Declaration
convertFunction moduleName { name, typeAnnotation } =
    let
        expression : Maybe Elm.Expression
        expression =
            case ( Node.value name, toSimpleType typeAnnotation ) of
                ( "call_", _ ) ->
                    Nothing

                ( "moduleName_", _ ) ->
                    Nothing

                ( "values_", _ ) ->
                    Nothing

                ( "annotation_", _ ) ->
                    Nothing

                ( "text", _ ) ->
                    Just text

                ( "map", _ ) ->
                    case moduleName of
                        [ "Html", "WithContext" ] ->
                            Just map

                        [ "Html", "WithContext", "Attributes" ] ->
                            Just mapAttribute

                        _ ->
                            Gen.Debug.todo ("no idea how to handle `map` in module " ++ String.join "." moduleName) |> Just

                ( "node", _ ) ->
                    case moduleName of
                        [ "Html", "WithContext" ] ->
                            Just node

                        [ "Html", "WithContext", "Keyed" ] ->
                            Just keyedNode

                        _ ->
                            Gen.Debug.todo ("no idea how to handle `node` in module " ++ String.join "." moduleName) |> Just

                ( n, Just (TFunction (TNamed [] "List" [ TNamed [] "Attribute" [ TVar "msg" ] ]) (TFunction (TNamed [] "List" [ _ ]) (TNamed [] "Html" [ TVar "msg" ]))) ) ->
                    case moduleName of
                        [ "Html", "WithContext" ] ->
                            Just <| genericNode n

                        [ "Html", "WithContext", "Keyed" ] ->
                            Just <| genericKeyedNode n

                        _ ->
                            Gen.Debug.todo ("no idea how to handle `" ++ n ++ "` in module " ++ String.join "." moduleName)
                                |> Just

                ( n, Just (TFunction s (TNamed [] "Attribute" [ TVar "msg" ])) ) ->
                    case moduleName of
                        [ "Html", "WithContext", "Attributes" ] ->
                            Just <| attribute1 s n

                        [ "Html", "WithContext", "Events" ] ->
                            Just <| event1 s n

                        _ ->
                            Gen.Debug.todo ("no idea how to handle `" ++ n ++ "` in module " ++ String.join "." moduleName)
                                |> Just

                ( n, Just (TFunction a (TFunction b (TNamed [] "Attribute" [ TVar "msg" ]))) ) ->
                    case moduleName of
                        [ "Html", "WithContext", "Attributes" ] ->
                            Just <| attribute2 a b n

                        [ "Html", "WithContext", "Events" ] ->
                            Just <| event2 a b n

                        _ ->
                            Gen.Debug.todo ("no idea how to handle `" ++ n ++ "` in module " ++ String.join "." moduleName)
                                |> Just

                ( n, Just ((TNamed [ "Json" ] "Decoder" [ _ ]) as t) ) ->
                    case moduleName of
                        [ "Html", "WithContext", "Events" ] ->
                            Just
                                (Elm.value
                                    { importFrom = [ "Html", "Events" ]
                                    , name = n
                                    , annotation = Just <| simpleTypeToAnnotation t
                                    }
                                )

                        _ ->
                            Gen.Debug.todo ("no idea how to handle `" ++ n ++ "` in module " ++ String.join "." moduleName)
                                |> Just

                ( n, t ) ->
                    Gen.Debug.todo ("no idea how to handle `" ++ n ++ "` of type " ++ Maybe.withDefault "?" (Maybe.map typeToString t) ++ " in module " ++ String.join "." moduleName)
                        |> Just
    in
    Maybe.map (Elm.declaration (Node.value name)) expression


attribute1 : SimpleType -> String -> Elm.Expression
attribute1 argType name =
    Elm.fn
        ( "arg", Just <| simpleTypeToAnnotation argType )
        (\arg ->
            Gen.Html.WithContext.Internal.make_.attribute
                (Elm.fn ( "_", Nothing )
                    (\_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "Html", "Attributes" ]
                                , name = name
                                , annotation = Nothing
                                }
                            )
                            [ arg ]
                    )
                )
                |> Elm.withType (Type.namedWith [ "Html", "WithContext" ] "Attribute" [ Type.var "context", Type.var "msg" ])
        )


event1 : SimpleType -> String -> Elm.Expression
event1 argType name =
    Elm.fn
        ( "arg", Just <| simpleTypeToAnnotation argType )
        (\arg ->
            Gen.Html.WithContext.Internal.make_.attribute
                (Elm.fn ( "_", Nothing )
                    (\_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "Html", "Events" ]
                                , name = name
                                , annotation = Nothing
                                }
                            )
                            [ arg ]
                    )
                )
                |> Elm.withType (Type.namedWith [ "Html", "WithContext" ] "Attribute" [ Type.var "context", Type.var "msg" ])
        )


attribute2 : SimpleType -> SimpleType -> String -> Elm.Expression
attribute2 argType1 argType2 name =
    Elm.fn2
        ( "arg1", Just <| simpleTypeToAnnotation argType1 )
        ( "arg2", Just <| simpleTypeToAnnotation argType2 )
        (\arg1 arg2 ->
            Gen.Html.WithContext.Internal.make_.attribute
                (Elm.fn ( "_", Nothing )
                    (\_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "Html", "Attributes" ]
                                , name = name
                                , annotation = Nothing
                                }
                            )
                            [ arg1, arg2 ]
                    )
                )
                |> Elm.withType (Type.namedWith [ "Html", "WithContext" ] "Attribute" [ Type.var "context", Type.var "msg" ])
        )


event2 : SimpleType -> SimpleType -> String -> Elm.Expression
event2 argType1 argType2 name =
    Elm.fn2
        ( "arg1", Just <| simpleTypeToAnnotation argType1 )
        ( "arg2", Just <| simpleTypeToAnnotation argType2 )
        (\arg1 arg2 ->
            Gen.Html.WithContext.Internal.make_.attribute
                (Elm.fn ( "_", Nothing )
                    (\_ ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "Html", "Events" ]
                                , name = name
                                , annotation = Nothing
                                }
                            )
                            [ arg1, arg2 ]
                    )
                )
                |> Elm.withType (Type.namedWith [ "Html", "WithContext" ] "Attribute" [ Type.var "context", Type.var "msg" ])
        )


simpleTypeToAnnotation : SimpleType -> Type.Annotation
simpleTypeToAnnotation type_ =
    case type_ of
        TUnit ->
            Type.unit

        TVar v ->
            Type.var v

        TFunction f t ->
            Type.function [ simpleTypeToAnnotation f ] (simpleTypeToAnnotation t)

        TNamed [ "Json" ] "Value" [] ->
            Type.namedWith [ "Json", "Encode" ] "Value" []

        TNamed [ "Json" ] "Decoder" args ->
            Type.namedWith [ "Json", "Decode" ] "Decoder" (List.map simpleTypeToAnnotation args)

        TNamed mod name args ->
            Type.namedWith mod name (List.map simpleTypeToAnnotation args)

        TTuple [ l, r ] ->
            Type.tuple (simpleTypeToAnnotation l) (simpleTypeToAnnotation r)

        TTuple [ l, m, r ] ->
            Type.triple (simpleTypeToAnnotation l) (simpleTypeToAnnotation m) (simpleTypeToAnnotation r)

        TRecord fields ->
            fields
                |> List.map (\( k, v ) -> ( k, simpleTypeToAnnotation v ))
                |> Type.record

        TTuple _ ->
            Type.var "wrong tuple"


map : Elm.Expression
map =
    Elm.fn2
        ( "f", Nothing )
        ( "child", Nothing )
        (\f n ->
            Gen.Html.WithContext.Internal.make_.html <|
                Elm.fn ( "context", Nothing )
                    (\context ->
                        Gen.Html.call_.map
                            f
                            (Gen.Html.WithContext.Internal.runHtml context n)
                    )
        )
        |> Elm.withType
            (Type.function [ Type.function [ Type.var "a" ] (Type.var "b") ]
                (Type.function
                    [ Type.namedWith
                        []
                        "Html"
                        [ Type.var "context", Type.var "a" ]
                    ]
                    (Type.namedWith
                        []
                        "Html"
                        [ Type.var "context", Type.var "b" ]
                    )
                )
            )


mapAttribute : Elm.Expression
mapAttribute =
    Elm.fn2
        ( "f", Nothing )
        ( "attr", Nothing )
        (\f n ->
            Gen.Html.WithContext.Internal.make_.attribute <|
                Elm.fn ( "context", Nothing )
                    (\context ->
                        Gen.Html.Attributes.call_.map
                            f
                            (Gen.Html.WithContext.Internal.runAttribute context n)
                    )
        )
        |> Elm.withType
            (Type.function [ Type.function [ Type.var "a" ] (Type.var "b") ]
                (Type.function
                    [ Type.namedWith
                        [ "Html", "WithContext" ]
                        "Attribute"
                        [ Type.var "context", Type.var "a" ]
                    ]
                    (Type.namedWith
                        [ "Html", "WithContext" ]
                        "Attribute"
                        [ Type.var "context", Type.var "b" ]
                    )
                )
            )


text : Elm.Expression
text =
    Elm.fn ( "content", Nothing )
        (\content ->
            Gen.Html.WithContext.Internal.make_.html <|
                Elm.fn ( "_", Nothing )
                    (\_ -> Gen.Html.call_.text content)
        )
        |> Elm.withType (Type.function [ Type.string ] (htmlAnnotation True))


keyedNode : Elm.Expression
keyedNode =
    Elm.fn3
        ( "name", Nothing )
        ( "attrs", Nothing )
        ( "children", Nothing )
        (\nodeName attrs children ->
            Gen.Html.WithContext.Internal.make_.html <|
                Elm.fn ( "context", Nothing )
                    (\context ->
                        Gen.Html.Keyed.call_.node
                            nodeName
                            (Gen.List.call_.map
                                (Elm.functionReduced "attr" <| Gen.Html.WithContext.Internal.runAttribute context)
                                attrs
                            )
                            (Gen.List.call_.map
                                (Elm.function
                                    [ ( "(key, child)", Nothing ) ]
                                    (\_ ->
                                        Elm.tuple
                                            (Elm.val "key")
                                            (Gen.Html.WithContext.Internal.runHtml context (Elm.val "child"))
                                    )
                                )
                                children
                            )
                    )
        )
        |> Elm.withType
            (Type.function
                [ Type.string
                , Type.list (attributeAnnotation False)
                , Type.list (Type.tuple Type.string (htmlAnnotation False))
                ]
                (htmlAnnotation False)
            )


node : Elm.Expression
node =
    Elm.fn3
        ( "name", Nothing )
        ( "attrs", Nothing )
        ( "children", Nothing )
        (\nodeName attrs children ->
            Gen.Html.WithContext.Internal.make_.html <|
                Elm.fn ( "context", Nothing )
                    (\context ->
                        Gen.Html.call_.node
                            nodeName
                            (Gen.List.call_.map
                                (Elm.functionReduced "attr" <| Gen.Html.WithContext.Internal.runAttribute context)
                                attrs
                            )
                            (Gen.List.call_.map
                                (Elm.functionReduced "child" <| Gen.Html.WithContext.Internal.runHtml context)
                                children
                            )
                    )
        )
        |> Elm.withType
            (Type.function
                [ Type.string
                , Type.list (attributeAnnotation True)
                , Type.list (htmlAnnotation True)
                ]
                (htmlAnnotation True)
            )


genericNode : String -> Elm.Expression
genericNode name =
    Elm.fn2
        ( "attrs", Nothing )
        ( "children", Nothing )
        (\attrs children ->
            Elm.apply (Elm.val "node")
                [ Elm.string name
                , attrs
                , children
                ]
        )
        |> Elm.withType
            (Type.function
                [ Type.list (attributeAnnotation True)
                , Type.list (htmlAnnotation True)
                ]
                (htmlAnnotation True)
            )


genericKeyedNode : String -> Elm.Expression
genericKeyedNode name =
    Elm.fn2
        ( "attrs", Nothing )
        ( "children", Nothing )
        (\attrs children ->
            Elm.apply (Elm.val "node")
                [ Elm.string name
                , attrs
                , children
                ]
        )
        |> Elm.withType
            (Type.function
                [ Type.list (attributeAnnotation False)
                , Type.list (Type.tuple Type.string (htmlAnnotation False))
                ]
                (htmlAnnotation False)
            )


typeToString : SimpleType -> String
typeToString st =
    case st of
        TUnit ->
            "()"

        TFunction f t ->
            "(" ++ typeToString f ++ " -> " ++ typeToString t ++ ")"

        TNamed mod name args ->
            String.join " " (String.join "." (mod ++ [ name ]) :: List.map typeToString args)

        TVar v ->
            v

        TTuple ts ->
            "(" ++ String.join ", " (List.map typeToString ts) ++ ")"

        TRecord fields ->
            "{ " ++ String.join ", " (List.map (\( k, v ) -> k ++ ":" ++ typeToString v) fields) ++ " }"


toSimpleType : Node.Node TypeAnnotation.TypeAnnotation -> Maybe SimpleType
toSimpleType ann =
    case Node.value ann of
        TypeAnnotation.FunctionTypeAnnotation f t ->
            Maybe.map2 TFunction (toSimpleType f) (toSimpleType t)

        TypeAnnotation.GenericType name ->
            Just <| TVar name

        TypeAnnotation.Typed name args ->
            Maybe.map
                (TNamed
                    (Node.value name |> Tuple.first)
                    (Node.value name |> Tuple.second)
                )
                (Maybe.Extra.traverse toSimpleType args)

        TypeAnnotation.Unit ->
            Just TUnit

        TypeAnnotation.Tupled lst ->
            Maybe.map
                TTuple
                (Maybe.Extra.traverse toSimpleType lst)

        TypeAnnotation.Record fields ->
            fields
                |> Maybe.Extra.traverse
                    (\fieldNode ->
                        let
                            ( name, value ) =
                                Node.value fieldNode
                        in
                        Maybe.map
                            (Tuple.pair <| Node.value name)
                            (toSimpleType value)
                    )
                |> Maybe.map TRecord

        _ ->
            Nothing


type SimpleType
    = TFunction SimpleType SimpleType
    | TNamed (List String) String (List SimpleType)
    | TVar String
    | TUnit
    | TTuple (List SimpleType)
    | TRecord (List ( String, SimpleType ))


htmlAnnotation : Bool -> Type.Annotation
htmlAnnotation sameModule =
    Type.namedWith
        (if sameModule then
            []

         else
            [ "Html.WithContext" ]
        )
        "Html"
        [ Type.var "context", Type.var "msg" ]


attributeAnnotation : Bool -> Type.Annotation
attributeAnnotation sameModule =
    Type.namedWith
        (if sameModule then
            []

         else
            [ "Html.WithContext" ]
        )
        "Attribute"
        [ Type.var "context", Type.var "msg" ]


parseHelper : String -> Result String RawFile.RawFile
parseHelper fragment =
    let
        helperCode : String
        helperCode =
            if String.startsWith "module " fragment then
                fragment

            else
                "module " ++ fragment

        exposingIndex : Int
        exposingIndex =
            String.indexes "exposing" helperCode
                |> List.head
                -- If the parsing succeeds the module _will_ have an `exposing` clause
                |> Maybe.withDefault -1

        moduleName : String
        moduleName =
            helperCode
                |> String.slice (String.length "module Gen.") (exposingIndex - 1)
    in
    Elm.Parser.parse helperCode
        |> Result.mapError (\_ -> "Error parsing the source file for " ++ moduleName)
