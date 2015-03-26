// FIXME: Replace this with pure GHCJS. It's just more laborious to do
// so.
var editor;
var AceRange = ace.require('ace/range').Range;
function makeEditor(e,onClick,onDblClick){
  editor = ace.edit(e.get(0));
  editor.setTheme("ace/theme/tomorrow");
  editor.getSession().setMode("ace/mode/haskell");
  editor.setReadOnly(true);
  editor.on("click", debounce(function(e) {
    var pos = e.getDocumentPosition();
    var s = editor.getSelection();
    var anchor = s.getSelectionAnchor();
    var lead = s.getSelectionLead();
    // Swap them to have the right order
    if ((anchor.row > lead.row) || (anchor.row == lead.row && anchor.column > lead.column)){
      var tmp = lead;
      lead = anchor;
      anchor = tmp;
    }
    if (onClick) onClick(
      { clientX: e.clientX,clientY: e.clientY,
        "startLine": anchor.row + 1,
        "startCol": anchor.column + 1,
        "endLine": lead.row + 1,
        "endCol": lead.column + 1
      });
  }));
  editor.on("dblclick", debounce(function(e) {
    var pos = e.getDocumentPosition();
    if (onDblClick) onDblClick({ clientX: e.clientX,clientY: e.clientY });
  }));
  return editor;
}

// Debounce the given function
function debounce(f){
  var t;
  return function(x){
    clearTimeout(t);
    t = setTimeout(function(){
      f(x);
    },100);
  };
}

function getSelectionRange(e){
  var r = e.getSelectionRange();
  return {"start-col": r.start.column +1 ,
          "start-line": r.start.row +1,
          "end-col": r.end.column +1,
          "end-line": r.start.row+1};
}
