src/Html/WithContext.elm: codegen/Gen/Platform.elm codegen/Generate.elm
	zsh -c "yarn elm-codegen run --output src --flags-from =(cat ~/.elm/0.19.1/packages/elm/html/1.0.0/src/{Html.elm,Html/*.elm})"
	perl -pi -e 's/{-\| - -}/{-|-}/' src/Html/*.elm src/Html/WithContext/*.elm

codegen/Gen/Platform.elm: codegen/helpers/Html/WithContext/Internal.elm
	yarn elm-codegen install
