(function() {
    var activeTabName;
    var main;
    var editor;
    var vsplitter;
    var hsplitter;
    var aceEditor;
    var aceOutput;
    var left;
    var errorwindow;
    var outputwindow;
    var errorMarkers = [];
    var lang;
    var resultLang;
    var outputMode;

    var hpos; // Position of horizontal splitter
    var vpos; // Position of vertical splitter
    
    function updateLayout() {
        var w = main.width();
        var h = main.height();
        var splitSize = 6;
        left.width(vpos - splitSize);
        outputwindow.css({ left: vpos,
                         width: w - vpos });
        vsplitter.css({ left: vpos - splitSize,
                        width: splitSize });
        editor.css({ height: hpos - splitSize });
        errorwindow.css({ height: h - hpos,
                     top: hpos });
        hsplitter.css({ top: hpos - splitSize,
                        height: splitSize });
        aceEditor.resize()
        aceOutput.resize()
    }

    function setInitialLayout() {
        vpos = main.width() / 2;
        hpos = main.height() - main.height() / 5;
    }

    var resizeTimer;
    function onWindowResizeHandler() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(updateLayout, 100);
    }

    function onVSplitterMouseDownHandler() {
        $(window).mousemove(function(event) {
            vpos = event.pageX - main.position().left;
            updateLayout();
        });
        $(window).mouseup(function() {
            $(window).unbind('mouseup');
            $(window).unbind('mousemove');
        });
    }
    
    function onLangClickHandler() {
        toggleLang();
    }
    function toggleLang() {
        lang = getOtherLang();
        updateLang();
    }
    function updateLang() {
        optionalLocalStorageSetItem("lang", lang);
        langButtonText.html(lang.toUpperCase());
        outputMode = null;
    }
    
    function onRunClickHandler() {
        resultLang = null;
        transferButton.hide();
        outputMode = null;
        runProgram('run');
    }

    function onInvertClickHandler() {
        resultLang = lang;
        outputMode = lang;
        transferButton.show();
        runProgram('invert');
    }

    function onTranslateClickHandler() {
        resultLang = getOtherLang(); 
        transferButton.show();
        outputMode = lang;
        runProgram('translate');
    }
    
    function onTransferClickHandler() {
        if (resultLang !== null) {
            aceEditor.getSession().setValue(aceOutput.getSession().getValue())
            lang = resultLang;
            updateLang();
            resultLang = null;
        }
        transferButton.hide();
    }
    function getOtherLang() {
        return(lang == 'srl' ? 'rl' : 'srl');
    }

    function onLogClickHandler() {
        optionalLocalStorageSetItem("log", logCheckBox.prop('checked'));
    }

    function onThemeSelectChange(e) {
        setTheme($('option:selected', e.target).val());
    }

    var themeTimer;
    function setTheme(theme) {
        var theme = theme || optionalLocalStorageGetItem('theme') || 'ace/theme/chrome';
        aceEditor.setTheme(theme);
        aceOutput.setTheme(theme)
        // TODO: change general CSS to match theme
        clearTimeout(themeTimer);
        themeTimer = setTimeout(updateColors, 100);
        optionalLocalStorageSetItem('theme', theme);
    }
    function updateColors() {
        // var style = aceEditor.renderer.scroller.style;
        // var bgColor = style.backgroundColor;
        // var textColor = style.color;
        // var hoverColor = style.gutter;
        // console.log(hoverColor)
        // navbar.css('background-color', bgColor)
        // // navbar.css('border-bottom', '4px solid ' + borderColor)
        // logo.css('color', textColor)
        // var buttons = $('#control button')
        // buttons.each(function(index) {
        //     $(this).css('background-color', textColor)
        //     $(this).css('color', bgColor)
        // });
        // buttons.hover(function() {
        //     $(this).css('background-color', hoverColor)
        // },function() {
        //     $(this).css('background-color', textColor)
        // });
        svgData.css('fill', logo.css('color'))
    }
    
    function onHSplitterMouseDownHandler() {
        $(window).mousemove(function(event) {
            hpos = event.pageY - main.position().top;
            updateLayout();
        });
        $(window).mouseup(function() {
            $(window).unbind('mouseup');
            $(window).unbind('mousemove');
        });
    }

    var runRequest = null;
    function runProgram(mode)
    {
        resetMarkers();
        if (runRequest !== null)
            return;
        disableEditor(aceEditor);
        aceOutput.getSession().setValue('');
        runRequest = $.ajax(
            { url: "/api",
              method: "POST",
              data: {
                  lang: lang,
                  mode: mode,
                  script: aceEditor.getValue(),
                  log: logCheckBox.prop('checked').toString()
              },
              timeout: 8000
            })
            .success( function(response) {
                handleRunResponse(response);
            })
            .fail(function(hxr, textStatus, errorThrown) {
                outputMode = null;
                errorwindow.html("Request failed with error: " + textStatus);
            })
            .always(function() { 
                enableEditor(aceEditor)
                updateMode();
                runRequest = null;
                errorwindow.scrollTop(0);
            });
    }

    function highlightErrors(row, col) {
        var aceRange = ace.require('ace/range').Range;
        var rowFrom = new Number(row)-1;
        var colFrom = 0;
        var rowTo   = new Number(row)-1;
        var colTo   = 9999;
        var type = "ace_error-marker"
        var marker = aceEditor
            .getSession()
            .addMarker(new aceRange(rowFrom, colFrom, rowTo, colTo),
                       type,
                       "text");
        errorMarkers.push(marker);
    }

    function resetMarkers() {
        $.each(errorMarkers, function(i, marker) {
            aceEditor.getSession().removeMarker(marker);
        });
        errorMarkers = [];
    }
    
    function handleRunResponse(response) {
        errorwindow.html('');
        if (response.error) {
            outputMode = null;
            // aceOutput.getSession().setValue(response.error);
            errorwindow.html('<pre><samp>' + response.error + '</samp></pre>');
            highlightErrors(response.loc_l, response.loc_c);
        } else
        {
            aceOutput.getSession().setValue(response.output);
        }
        
        if (response.log !== null) {
            errorwindow.append('<pre><samp>Execution trace:</pre></samp>' )
            $.each(response.log, function(i,message) {
                errorwindow.append('<pre><samp>' + message + '</pre></samp>' )
            })
        }
    }
    
    function optionalLocalStorageGetItem(key) {
        try {
            return localStorage.getItem(key);
        } catch(e) {
            return null;
        }
    }

    function optionalLocalStorageSetItem(key, value) {
        try {
            window.localStorage.setItem(key, value);
        } catch(e) {
            // ignore
        }
    }
    
    function updateMode() {
        aceOutput.getSession().setMode(outputMode ? "ace/mode/rlsrl" : null);
    }

    function getQueryParameters() {
        var a = window.location.search.substr(1).split('&');
        if (a === "") return {};
        var b = {};
        for (var i = 0; i < a.length; i++) {
            var p = a[i].split('=');
            if (p.length != 2) continue;
            b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
        }
        return b;
    }

    function populateThemeSelect(themeList, themeSelect)
    {
        $.each(themeList.themes, function(i, theme) {
            $('<option/>',
              { value : theme.theme,
                selected : aceEditor.getTheme() === theme.theme })
                .text(theme.caption)
                .appendTo(themeSelect);
        });
    }
    
    function disableEditor(editor) {
        editor.setReadOnly(true);
        editor.renderer.$cursorLayer.element.style.display = "none";
        editor.setHighlightActiveLine(false);
    }
    function enableEditor(editor) {
        editor.setReadOnly(false);
        editor.renderer.$cursorLayer.element.style.display = "";
        editor.setHighlightActiveLine(true);
    }
    
    $().ready(function() {
        main            = $("#main");
        editor          = $("#editor");
        vsplitter       = $("#vsplitter");
        hsplitter       = $("#hsplitter");
        left            = $("#left");
        errorwindow     = $("#errorwindow");
        outputwindow    = $("#outputwindow");
        langButton      = $("#lang")
        langButtonText  = $("#langtext")
        runButton       = $("#run");
        invertButton    = $("#invert");
        translateButton = $("#translate");
        transferButton  = $("#transfer"); transferButton.hide();
        saveButton      = $("#save");
        shareButton     = $("#share");
        openButton      = $("#open");
        reportButton    = $("#report");
        helpButton      = $("#help");
        logCheckBox     = $("#log");
        themeSelect     = $("#theme");
        navbar          = $("#control");
        logo            = $("#logo");
        svgData         = $("#haskell-path");
        
        aceEditor = ace.edit("editor");
        aceOutput = ace.edit("outputwindow")
        setTheme();

        // bindings
        aceEditor.commands.addCommand({
            name: 'RunCommand',
            bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
            exec: function(editor) {
                onRunClickHandler();
            },
            readOnly: true
        });
        aceEditor.commands.addCommand({
            name: 'InvertCommand',
            bindKey: {win: 'Shift-Enter',  mac: 'Shift-Enter'},
            exec: function(editor) {
                onInvertClickHandler();
            },
            readOnly: true
        });
        aceEditor.commands.addCommand({
            name: 'TranslateCommand',
            bindKey: {win: 'Ctrl-Shift-Enter',  mac: 'Command-Shift-Enter'},
            exec: function(editor) {
                onTranslateClickHandler();
            },
            readOnly: true
        });
        aceEditor.commands.addCommand({
            name: 'LangCommand',
            bindKey: {win: 'Ctrl-Space',  mac: 'Command-Space'},
            exec: function(editor) {
                onLangClickHandler();
            },
            readOnly: true
        });
        
        // configure output window
        disableEditor(aceOutput)
        aceOutput.renderer.setShowGutter(false);
        aceOutput.getSession().setValue('');

        // set themelist
        var themeList = ace.require("ace/ext/themelist");
        populateThemeSelect(themeList, themeSelect);

        // set layout
        setInitialLayout();
        updateLayout();

        // define elements
        $(window).resize(onWindowResizeHandler);
        vsplitter.mousedown(onVSplitterMouseDownHandler);
        hsplitter.mousedown(onHSplitterMouseDownHandler);
        langButton.click(onLangClickHandler);
        runButton.click(onRunClickHandler);
        invertButton.click(onInvertClickHandler);
        translateButton.click(onTranslateClickHandler);
        transferButton.click(onTransferClickHandler);
        helpButton.click(function() { window.open("help.html");  });
        logCheckBox.click(onLogClickHandler);
        themeSelect.change(onThemeSelectChange);

        // set lang value
        query = getQueryParameters();
        if ("lang" in query && (query.lang == 'rl' || query.lang == 'srl'))
        {
            lang = query.lang
        } else {
            var _lang = optionalLocalStorageGetItem("lang");
            if (_lang !== null && (_lang == 'rl' || _lang == 'srl')) {
                lang = _lang
            }
            else {
                lang = 'srl'
            }
        }
        // set code in editor
        if ("script" in query)
        {
            aceEditor.getSession().setValue(query.script);
        } else {
            var script = optionalLocalStorageGetItem("script");
            if (script !== null) {
                aceEditor.getSession().setValue(script);
            }
            else {
                if (lang == 'srl') {
                    aceEditor.getSession().setValue(
`// Compute the nth fibonacci pair

int n
int v int w

n ^= 16
w ^= 1
from (v = 0) do
  v += w
  swap v w
  n -= 1
loop .
until (n = 0 || v > w)`
                    );
                }
                else if (lang == 'rl') {
                    aceEditor.getSession().setValue(
`// Compute the nth fibonacci pair

int n
int v int w

start: entry
  n ^= 16
  w ^= 1
goto loop

loop: fi (v = 0) start loop
  v += w
  swap v w
  n -= 1
if (n = 0 || v > w) end loop

end: from loop
exit`
                    );
                }
                else {
                    aceEditor.getSession().setValue('');
                }
            }
        }
        
        // what highlighting to use in output window
        aceEditor.getSession().setMode("ace/mode/rlsrl");
        outputMode = null;
        updateMode();
        updateLang();

        // set whether to log or not
        var log = optionalLocalStorageGetItem("log");
        if (log == "true")
            logCheckBox.prop('checked', true);
        
        aceEditor.getSession().on("change", function() {
            var script = aceEditor.getSession().getValue();
            optionalLocalStorageSetItem("script", script);
            resetMarkers();
        });
    });
})();
