use "strict";

function renderRud(pos) {
    return function() {
      var rud = document.getElementById('sprite');
      rud.style.left = pos.x + 'px';
      rud.style.top  = pos.y + 'px';
    };
};

exports.renderRud = renderRud;
