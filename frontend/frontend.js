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
    // position of horizontal splitter as a fraction of whole page
    var hpos;
    // position of vertical splitter as a fraction of whole page
    var vpos;
    
    function updateLayout() {
        var w = main.width();
        var h = main.height();
        var splitSize = 6;
        var outputLeft = w * vpos;
        var outputWidth = w - outputLeft;
        var editorHeight = h * hpos
        left.width(outputLeft - splitSize);
        outputwindow.css({ left: outputLeft,
                         width: outputWidth});
        vsplitter.css({ left: outputLeft - splitSize,
                        width: splitSize });
        editor.css({ height: editorHeight - splitSize });
        errorwindow.css({ height: h - editorHeight,
                     top: editorHeight });
        hsplitter.css({ top: editorHeight - splitSize,
                        height: splitSize });
        aceEditor.resize()
        aceOutput.resize()
    }

    function setInitialLayout() {
        vpos = optionalLocalStorageGetItem("vpos");
        if (vpos == null) {
            vpos = .5;
        }
        hpos = optionalLocalStorageGetItem("hpos");
        if (hpos == null) {
            hpos = .8;
        }
    }

    var resizeTimer;
    function onWindowResizeHandler() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(updateLayout, 100);
    }

    const minFrac = .01; const maxFrac = 1 - minFrac;
    function normalizeFrac(frac) {
        if (frac < minFrac) return minFrac;
        if (frac > maxFrac) return maxFrac;
        return frac;
    }
    function onVSplitterMouseDownHandler() {
        var w = main.width();
        $(window).mousemove(function(event) {
            vpos = normalizeFrac((event.pageX - main.position().left) / w);
            optionalLocalStorageSetItem("vpos", vpos);
            updateLayout();
        });
        $(window).mouseup(function() {
            $(window).unbind('mouseup');
            $(window).unbind('mousemove');
        });
    }
    function onHSplitterMouseDownHandler() {
        var h = main.height();
        $(window).mousemove(function(event) {
            hpos =  normalizeFrac((event.pageY - main.position().top) / h);
            optionalLocalStorageSetItem("hpos", hpos);
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
        showsTransferButton = false;
        outputMode = null;
        runProgram('run');
    }

    function onInvertClickHandler() {
        resultLang = lang;
        outputMode = lang;
        showsTransferButton = true;
        runProgram('invert');
    }

    function onTranslateClickHandler() {
        resultLang = getOtherLang(); 
        showsTransferButton = true;
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
        showsTransferButton = false;
        updateTransferButton();
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

    function setTheme(theme) {
        var theme = theme || optionalLocalStorageGetItem('theme') || 'ace/theme/chrome';
        aceEditor.setTheme(theme);
        aceOutput.setTheme(theme)
        optionalLocalStorageSetItem('theme', theme);
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
            .done( function(response) {
                handleRunResponse(response);
            })
            .fail(function(hxr, textStatus, errorThrown) {
                outputMode = null;
                errorwindow.html('<pre><samp>Request failed with error: ' + textStatus + '</samp></pre>');
            })
            .always(function() { 
                enableEditor(aceEditor)
                updateTransferButton();
                updateMode();
                runRequest = null;
                errorwindow.scrollTop(0);
            });
    }
    
    function updateTransferButton() {
        if (showsTransferButton) {
            transferButton.show();
        }
        else {
            transferButton.hide();
        }
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
            showsTransferButton = false;
            transferButton.hide();
            outputMode = null;
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
