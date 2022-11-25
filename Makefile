generated/Html/WithContext.elm: codegen/Gen/Platform.elm
	zsh -c "elm-codegen run --flags-from =(cat ~/.elm/0.19.1/packages/elm/html/1.0.0/src/{Html.elm,Html/*.elm})"

codegen/Gen/Platform.elm: codegen/helpers/Html/WithContext/Internal.elm
	elm-codegen install
