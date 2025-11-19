module RudderTree exposing
    ( TreeNodeId, TreeNode, ViewTree(..), BranchFoldStatus(..)
    , sortTree
    , Model, Msg
    , view, update, init, initFlatTree
    )

{-| A Tree component library with the [jstree] library behavior.
It does not aim to replicate the library in Elm, and only has some minimalistic features.
The event handling is purely based on the Elm architecture, with the [Nested TEA][nested-tea] pattern.

[jstree]: https://www.jstree.com/
[nested-tea]: https://sporto.github.io/elm-patterns/architecture/nested-tea.html


# Data types and constructors

@docs TreeNodeId, TreeNode, ViewTree, BranchFoldStatus
@docs sortTree


# TEA

@docs Model, Msg
@docs view, update, init, initFlatTree

-}

import Dict exposing (Dict)
import Html exposing (Html, a, div, i, li, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Lazy
import Ordering exposing (Ordering)


{-| The type marker to identify a node in the tree. Identifier is very useful for event handling, and performance optimization
-}
type alias TreeNodeId =
    String


{-| Representation of a node of a tree, be it a branch or a leaf
-}
type alias TreeNode =
    { id : TreeNodeId
    , name : String

    -- , description : Maybe String  -- for hover tooltip description ?
    }


{-| The tree datatype that maps to a "jstree" representation and view :

  - there is always a root is implicit, and that it is not displayed as a node, but can have children
  - there can be "branch" nodes which may contain other branches and leaves, and which can be folded
  - there can be "leaf" nodes which do not contain children and thus cannot be folded

-}
type ViewTree
    = Root (List ViewTree)
    | Branch TreeNode (List ViewTree) BranchFoldStatus
    | Leaf TreeNode


{-| State of branch : open or closed
-}
type BranchFoldStatus
    = Open
    | Closed


{-| The model of the tree, the root tree must always be Root
FIXME: it needs to be an opaque type then
-}
type alias Model =
    { rootTree : ViewTree
    }


{-| The internal message type
-}
type Msg
    = ToggleBranchStatus TreeNodeId


{-| Initial tree without any elements
-}
init : Model
init =
    { rootTree = Root []
    }


toggle : BranchFoldStatus -> BranchFoldStatus
toggle status =
    case status of
        Open ->
            Closed

        Closed ->
            Open


branchFoldStatusText : BranchFoldStatus -> String
branchFoldStatusText status =
    case status of
        Open ->
            "open"

        Closed ->
            "closed"


{-| Recursively search for the branch that needs to be toggled from root,
and only modify the targeted branch
-}
toggleBranchStatus : TreeNodeId -> ViewTree -> ViewTree
toggleBranchStatus forId tree =
    case tree of
        Root t ->
            Root (List.map (toggleBranchStatus forId) t)

        Branch n m status ->
            if forId == n.id then
                Branch n m (toggle status)

            else
                Branch n (List.map (toggleBranchStatus forId) m) status

        other ->
            other


updateTree : (ViewTree -> ViewTree) -> Model -> Model
updateTree f ({ rootTree } as model) =
    { model | rootTree = f rootTree }


{-| The update function to call in the parent :

    RudderTreeMsg treeMsg ->
        let
            ( updatedModel, cmd, outMsg ) =
                update treeMsg model.treeModel

        in
        ( { model | treeModel = updatedModel }, cmd )

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleBranchStatus id ->
            ( model |> updateTree (toggleBranchStatus id), Cmd.none )


{-| Elements in the tree are sorted by lexicographical order by default in the tree.
FIXME: is there any reason for that ? Is it for performance, or is it historical ?
-}
viewTreeRec : ViewTree -> List (Html Msg)
viewTreeRec tree =
    case tree of
        Root t ->
            --FIXME: if tree is empty, and there are filters : "no tree matches your filter" ?
            t |> List.concatMap viewTreeRec

        Branch { id, name } subTrees status ->
            [ Html.Lazy.lazy4 viewBranch id name subTrees status
            ]

        Leaf { name } ->
            --FIXME this is where there are many possible UIs for the leaf
            --FIXME : lost onClick on child anchor
            [ li [ class "jstree-node jstree-leaf" ]
                [ i [ class "jstree-icon jstree-ocl" ] []
                , a [ class "jstree-anchor" ]
                    [ i [ class "jstree-icon jstree-themeicon fa fa-sitemap jstree-themeicon-custom" ] []
                    , span [ class "treeGroupName" ]
                        [ text name
                        ]
                    ]
                ]
            ]


viewBranch : TreeNodeId -> String -> List ViewTree -> BranchFoldStatus -> Html Msg
viewBranch id name subTrees status =
    li [ class ("jstree-node jstree-" ++ branchFoldStatusText status) ]
        [ i [ class "jstree-icon jstree-ocl", onClick (ToggleBranchStatus id) ] []
        , a [ class "jstree-anchor" ]
            [ i [ class "jstree-icon jstree-themeicon jstree-themeicon-custom fa fa-folder" ] []
            , span [ class "treeGroupCategoryName" ] [ text name ]
            ]
        , ul [ class "jstree-children" ] (subTrees |> List.concatMap viewTreeRec)
        ]


{-| View with a "jstree" structure
-}
view : Model -> Html Msg
view { rootTree } =
    div [ class "jstree jstree-default jstree-groups" ]
        [ ul [ class "jstree-container-ul jstree-children" ]
            (viewTreeRec rootTree)
        ]


ordering : Ordering ViewTree
ordering =
    Ordering.byRank
        (\tree ->
            case tree of
                Root _ ->
                    1

                Branch _ _ _ ->
                    2

                Leaf _ ->
                    3
        )
        (\x y ->
            case ( x, y ) of
                ( Branch n1 _ _, Branch n2 _ _ ) ->
                    Ordering.natural n1.name n2.name

                ( Leaf n1, Leaf n2 ) ->
                    Ordering.natural n1.name n2.name

                _ ->
                    Ordering.noConflicts
        )


{-| Sort the tree recursively according to the ordering.
Only branches need to be sorted together, and leaf are sorted

BE CAREFUL: only use it for initializers or setters that are called not so often,
as it may be costly to sort a huge tree
(e.g. if filtering nodes by user input, should debounce before sorting the filtered tree)

-}
sortTree : ViewTree -> ViewTree
sortTree tree =
    case tree of
        Branch n subs s ->
            Branch n (subs |> List.sortWith ordering |> List.map sortTree) s

        other ->
            other


{-| Build only a flat tree, with some known branches, and their leaves at the level below
-}
buildFlatTree : Dict TreeNodeId ( TreeNode, List TreeNode, BranchFoldStatus ) -> List ViewTree
buildFlatTree tree =
    tree
        |> Dict.values
        |> List.map (\( n, sub, status ) -> Branch n (List.map Leaf sub) status)


{-| Constructor to initialize a `Model` from a flat tree i.e with only leaves
-}
initFlatTree : Dict TreeNodeId ( TreeNode, List TreeNode, BranchFoldStatus ) -> Model
initFlatTree tree =
    { rootTree = Root (buildFlatTree tree)
    }
