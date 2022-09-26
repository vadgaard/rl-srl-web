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
    var outputMode;

    var hpos; // Position of horizontal splitter
    var vpos; // Position of vertical splitter

    function playSound(sound) {
        var audio = new Audio(sound);
        audio.play();
    }
    
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
        function hideVWindows() {
            left.hide();
            outputwindow.hide();
        }
        function showVWindows() {
            left.show();
            outputwindow.show();
        }        
        hideVWindows();
        $(window).mousemove(function(event) {
            vpos = event.pageX - main.position().left;
            updateLayout();
        });
        $(window).mouseup(function() {
            $(window).unbind('mouseup');
            $(window).unbind('mousemove');
            showVWindows();
        });
    }
    
    function onLangClickHandler() {
        if (lang == "srl") lang = "rl";
        else lang = "srl"
        optionalLocalStorageSetItem("lang", lang);
        updateLang();
    }
    function updateLang() {
        langButtonText.html(lang.toUpperCase());
        outputMode = null;
    }
    
    function onRunClickHandler() {
        outputMode = null;
        runProgram('run');
        updateMode();
    }

    function onInvertClickHandler() {
        outputMode = lang;
        runProgram('invert');
        updateMode();
    }

    function onTranslateClickHandler() {
        outputMode = lang
        runProgram('translate');
        updateMode();
    }

    function onReportClickHandler() {
        var m1 = "ZG9sbGVAZ";
        var m2 = "GlrdS5kaw==";
        var a = document.createElement('a');
        var mailto = 'mailto:' + window.atob(m1 + m2);
        mailto += '?subject=Bug%20report';
        mailto += '&body=';
        mailto +=
          encodeURIComponent("\n\n== Proof script causing unexpected behavior: ==\n\n");
        mailto += encodeURIComponent(aceEditor.getValue());
        a.setAttribute('href', mailto);
        a.style.cssText = "visibility:hidden;";
        document.body.appendChild(a);
        a.click();
        setTimeout( function() { document.body.removeChild(a) }, 1 );
    }

    function onEnhanceClickHandler() {
        optionalLocalStorageSetItem("enhance", enhanceCheckBox.prop('checked'));
    }
    
    function onRlClickHandler() {
        optionalLocalStorageSetItem("lang", 'rl');
    }
    function onSrlClickHandler() {
        optionalLocalStorageSetItem("lang", 'srl');
    }

    function onThemeSelectChange(e) {
        setTheme($('option:selected', e.target).val());
    }

    function setTheme(theme) {
        var theme = theme || optionalLocalStorageGetItem('theme') || 'ace/theme/chrome';
        aceEditor.setTheme(theme);
        aceOutput.setTheme(theme)
        // TODO: change general CSS to match theme
        svgData.css('fill', logo.css('color'))
        optionalLocalStorageSetItem('theme', theme);
    }
    
    function onHSplitterMouseDownHandler() {
        function hideHWindows() {
            errorwindow.hide();
            outputwindow.hide();
        }        
        function showHWindows() {
            errorwindow.show();
            outputwindow.show();
        }

        hideHWindows();
        $(window).mousemove(function(event) {
            hpos = event.pageY - main.position().top;
            updateLayout();
        });
        $(window).mouseup(function() {
            $(window).unbind('mouseup');
            $(window).unbind('mousemove');
            showHWindows();
        });
    }

    var checkRequest = null;
    function runProgram(mode)
    {
        resetMarkers();
        if (checkRequest !== null)
            return;
        disableEditor(aceEditor);
        aceOutput.getSession().setValue('');
        checkRequest = $.ajax(
            { url: SERVER_URL + "/api",
              method: "POST",
              data: {
                  lang: lang,
                  mode: mode,
                  script: aceEditor.getValue(),
              },
              timeout: 8000
            })
            .success( handleCheckResponse )
            .fail(function(hxr, textStatus, errorThrown) {
                errorwindow.html("Request failed with error: " + textStatus);
                errorwindow.scrollTop(errorwindow.prop("scrollHeight"));
            })
            .always(function() { 
                enableEditor(aceEditor)
                checkRequest = null;
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
    
    function handleCheckResponse(response) {
        // outputwindow.html('');
        // aceOutput.getSession().setValue('');
        errorwindow.html('');
        if (response.error) {
            errorwindow.html('<pre><samp>' + response.error + '</samp></pre>');
            errorwindow.scrollTop(errorwindow.prop("scrollHeight"));
            highlightErrors(response.loc_l, response.loc_c);
            if (enhanceCheckBox.prop('checked'))
                playSound('trombone.wav');
            return;
        } else
        {
            // outputwindow.html('<pre><samp>' + response.output + '</samp></pre>');
            aceOutput.getSession().setValue(response.output);
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
        aceEditor.getSession().setMode("ace/mode/" + lang);
        if (outputMode !== null)
            aceOutput.getSession().setMode("ace/mode/" + outputMode);
        else
            aceOutput.getSession().setMode(null);
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
        saveButton      = $("#save");
        shareButton     = $("#share");
        openButton      = $("#open");
        reportButton    = $("#report");
        helpButton      = $("#help");
        printButton     = $("#print");
        enhanceCheckBox = $("#enhance");
        themeSelect     = $("#theme");
        navbar          = $("#control");
        logo            = $("#logo");
        svgData         = $("#haskell-path");
        
        aceEditor = ace.edit("editor");
        aceOutput = ace.edit("outputwindow")
        setTheme();
        aceEditor.commands.addCommand({
            name: 'CheckCommand',
            bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
            exec: function(editor) {
                runProgram();
            },
            readOnly: true
        });
        disableEditor(aceOutput)
        aceOutput.renderer.setShowGutter(false);
        aceOutput.getSession().setValue('');

        var themeList = ace.require("ace/ext/themelist");
        populateThemeSelect(themeList, themeSelect);

        setInitialLayout();
        updateLayout();

        $(window).resize(onWindowResizeHandler);
        vsplitter.mousedown(onVSplitterMouseDownHandler);
        hsplitter.mousedown(onHSplitterMouseDownHandler);
        langButton.click(onLangClickHandler);
        runButton.click(onRunClickHandler);
        invertButton.click(onInvertClickHandler);
        translateButton.click(onTranslateClickHandler);
        reportButton.click(onReportClickHandler);
        helpButton.click(function() { window.open("help.html");  });
        enhanceCheckBox.click(onEnhanceClickHandler);
        themeSelect.change(onThemeSelectChange);

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
        
        outputMode = null;
        updateMode();
        updateLang();

        var enhance = optionalLocalStorageGetItem("enhance");
        if (enhance == "true")
            enhanceCheckBox.prop('checked', true);
        
        aceEditor.getSession().on("change", function() {
            var script = aceEditor.getSession().getValue();
            optionalLocalStorageSetItem("script", script);
            resetMarkers();
        });
    });
})();
