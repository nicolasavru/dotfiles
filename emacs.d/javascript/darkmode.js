// Based on https://stackoverflow.com/questions/4766201/javascript-invert-color-on-all-elements-of-a-page
'use strict';

// var body_style_display = getComputedStyle(document.body)['display'];
// document.body.style.display = "none";

function invertColors(){
  var colorProperties = ['color','background-color'];
  Array.prototype.slice.call(document.querySelectorAll('*')).reverse().forEach(function (el) {
    var styleval = null, color = null;
    for(var prop in colorProperties){
      prop = colorProperties[prop];
      styleval = getComputedStyle(el)[prop]
      if(styleval == undefined){
        continue;
      }

      // TODO: continue on unrecognized color format.
      color = d3.color(styleval);

      // If the background is transparent black (which is displayed by the
      // browser as white), set it to opaque white so that it gets inverted to
      // opaque black. Unless the foreground color is also black, in which case
      // leave it be to avoid black text on a black background.
      if(prop == 'background-color' &&
         xah_is_obj_equal(color, d3.rgb(0, 0, 0, 0)) &&
         !xah_is_obj_equal(d3.color(getComputedStyle(el)['color']),
                           d3.rgb(0, 0, 0))){
        color = d3.rgb([255, 255, 255]);
      }

      color = d3.jch(color);
      color.J = 100 - color.J;

      // color = d3.jab(color);
      // color.J = 100 - color.J;

      // color = d3.hsl(color)
      // color.l = 1 - color.l

      // color = d3.hsv(color)
      // color.v = 1 - color.v

      // color = d3.lab(color)
      // color.l = 100 - color.l

      // color = d3.hcl(color)
      // color.l = 100 - color.l

      // color = d3.cubehelix(color)
      // color.l = 1 - color.l

      el.style[prop] = color.toString();
      color = null;
      styleval = null;
    }
  });
  
  // https://stackoverflow.com/questions/22095529/hiding-everything-until-the-page-has-finished-loading
  // Hide body in mu4e~write-body-to-html, unhide it here.
  document.body.style.display = "inline";
}

function load_script(src, callback){
  var s = document.createElement('script');
  s.onload = callback;
  s.src = src;
  document.getElementsByTagName('head')[0].appendChild(s);
}

function load_scripts(srcs, callback){
  if(srcs.length == 1){
    load_script(srcs.pop(), callback);
  }
  else {
    var src = srcs.pop()
    load_scripts(srcs, function () {
      load_script(src, callback)
    });
  }
}

var deps = [
  // 'https://getfirebug.com/releases/lite/1.2/firebug-lite-compressed.js',
  // 'https://d3js.org/d3-color.v1.min.js',
  '/home/avru/.emacs.d/javascript/d3-color/build/d3-color.js',
  // 'https://unpkg.com/d3-cam02@0.1.5/build/d3-cam02.js',
  '/home/avru/.emacs.d/javascript/d3-cam02/build/d3-cam02.js',
  '/home/avru/.emacs.d/javascript/d3-hsv/build/d3-hsv.js',
  '/home/avru/.emacs.d/javascript/xah_object_equality.js',
  // 'https://cdnjs.cloudflare.com/ajax/libs/chroma-js/1.3.7/chroma.min.js',
];

load_scripts(deps, invertColors);

// window.load(function() {
//   document.getElementsByTagName('body')[0].fadeIn('slow');
//   // $("body").fadeIn("slow");
// });

// document.addEventListener("DOMContentLoaded", function(event) {
//   document.getElementsByTagName('body')[0].fadeIn('slow');
// });

// Chroma.js version
//
// function invertColors(){
//   var colorProperties = ['color','background-color'];
//   Array.prototype.slice.call(document.querySelectorAll('*')).reverse().forEach(function (el) {
//     var styleval = null, color = null;
//     for(var prop in colorProperties){
//       prop = colorProperties[prop];
//       styleval = getComputedStyle(el)[prop]
//       if(styleval == undefined){
//         continue;
//       }

//       // TODO: continue on unrecognized color format.
//       color = chroma(styleval);

//       // If the background is transparent black (which is displayed by the
//       // browser as white), set it to opaque white so that it gets inverted to
//       // opaque black. Unless the foreground color is also black, in which case
//       // leave it be to avoid black text on a black background.
//       if(prop == 'background-color' &&
//          xah_is_array_equal(color.rgba(), [0, 0, 0, 0]) &&
//          !xah_is_array_equal(chroma(getComputedStyle(el)['color']).rgb(),
//                              [0, 0, 0])){
//         color = chroma([255, 255, 255]).alpha(1);
//       }

//       // color = color.set('hsl.l', 1 - color.get('hsl.l'))
//       // color = color.set('hsv.v', 1 - color.get('hsv.v'))
//       // color = color.set('hsi.i', 1 - color.get('hsi.i'))
//       color = color.set('lab.l', 100 - color.get('lab.l'))
//       //color = color.luminance(1 - color.luminance(), 'hsl')
//       // color = color.luminance(0.75*color.luminance(), 'hsl')

//       el.style[prop] = color.css();
//       color = null;
//       styleval = null;
//     }
//   });
// }
