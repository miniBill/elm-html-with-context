src/Html/WithContext.elm: codegen/Gen/Platform.elm codegen/Generate.elm
	rm -rf src
	zsh -c "elm-codegen run --flags-from =(cat ~/.elm/0.19.1/packages/elm/html/1.0.0/src/{Html.elm,Html/*.elm})"
	mv generated src

codegen/Gen/Platform.elm: codegen/helpers/Html/WithContext/Internal.elm
	elm-codegen install
