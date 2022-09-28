frontend.css : less/frontend.less
	lessc less/frontend.less frontend/frontend.css

build : frontend.css
	stack build

run : frontend.css
	stack run