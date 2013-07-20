Main: Main.hs Application.hs Routes.hs MustacheTemplates.hs PathHelpers.hs Amount.hs PathFind.hs Websocket.hs
	ghc -O2 -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	routeGenerator -r -m Application -n 2 $< > $@

PathHelpers.hs: routes
	routeGenerator -p -n 2 $< > $@

MustacheTemplates.hs: Records.hs view/liquidityCheck.mustache
	mustache2hs -m Records.hs view/liquidityCheck.mustache Liquidity > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main Routes.hs PathHelpers.hs MustacheTemplates.hs
