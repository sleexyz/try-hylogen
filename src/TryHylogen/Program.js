"use strict";

// module Program

var React = require("react");
var PropTypes = React.PropTypes;
var Pux = require("purescript-pux");
var ReactDOM = require("react-dom");

// import Audio from "./Audio";

var vsSource = " attribute vec3 aPosition;\nvarying vec2 uvN;\nvoid main() {\n  gl_Position = vec4(aPosition, 1.0);\n  uvN = aPosition.xy;\n}";

var fsHeader = "precision mediump float;\nconst float pi = 3.141592653589793238462643383;\nuniform float time;\nuniform vec2 mouse;\nuniform vec2 resolution;\nuniform vec4 audio;\nuniform sampler2D backBuffer;\nuniform sampler2D channel1;\nvarying vec2 uvN;\nvec2 uv() {\n  return 0.5 * uvN  + 0.5;\n}";

var Program = React.createClass({
  propTypes: {
    vsSource: PropTypes.string,
    fsSource: PropTypes.string.isRequired,
    width: PropTypes.number,
    height: PropTypes.number,
    animation: PropTypes.bool
  },
  getDefaultProps: function getDefaultProps() {
    return {
      width: Math.max(window.innerHeight, window.innerWidth),
      height: Math.max(window.innerHeight, window.innerWidth),
      vsSource: vsSource,
      animation: true
    };
  },
  componentDidMount: function componentDidMount() {
    var canvas = this.canvas;
    var gl = this.gl = canvas.getContext("webgl");

    var state = this.state_ = {};
    state.animationFrameRequest = null;
    state.bit = 0;
    state.fb = [null, null];
    state.time0 = new Date() / 1000;
    /* state.audioCallback = null;*/

    function setMouse(event) {
      var r = event.target.getBoundingClientRect();
      state.mouse.x = (event.clientX - r.left) / (r.right - r.left) * 2 - 1;
      state.mouse.y = (event.clientY - r.bottom) / (r.top - r.bottom) * 2 - 1;
    };
    /* canvas.onmousedown = (event) => setMouse(event, 1); */
    /* canvas.onmouseup = (event) => setMouse(event, 0); */

    canvas.onmousemove = setMouse;
    state.mouse = { x: 0, y: 0 };

    state.audio = { low: 0.0, mid: 0.0, upper: 0.0, high: 0.0 };

    /* state.audioCallback = Audio.addCallback(function(bands) {
     *   state.audio.low = bands.low;
     *   state.audio.mid = bands.mid;
     *   state.audio.upper = bands.upper;
     *   state.audio.high = bands.high;
     * });*/

    this.loadProgram();
  },
  loadProgram: function loadProgram() {
    var gl = this.gl;
    var state = this.state_;
    var WIDTH = this.props.width;
    var HEIGHT = this.props.height;

    // compileShader :: (gl, source, shaderType) -> Shader
    // throws Error on compilation error

    function compileShader(gl, source, shaderType) {
      // assert(shaderType === gl.FRAGMENT_SHADER || shaderType === g.VERTEXT_SHADER);

      var shader = gl.createShader(shaderType);

      gl.shaderSource(shader, source);
      gl.compileShader(shader);

      var success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
      if (!success) {
        console.log(source);
        throw "could not compile shader:" + gl.getShaderInfoLog(shader);
      }

      return shader;
    };

    var vs = compileShader(gl, this.props.vsSource, gl.VERTEX_SHADER);
    var fs = compileShader(gl, fsHeader + "\n" + this.props.fsSource, gl.FRAGMENT_SHADER);

    var program = gl.createProgram();

    gl.attachShader(program, vs);
    gl.attachShader(program, fs);

    gl.linkProgram(program);

    var success = gl.getProgramParameter(program, gl.LINK_STATUS);
    if (!success) {
      throw "program failed to link:" + gl.getProgramInfoLog(program);
    }
    gl.useProgram(program);

    // Create a square as a strip of two triangles.
    gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, 1, 0, 1, 1, 0, -1, -1, 0, 1, -1, 0]), gl.STATIC_DRAW);

    // Assign attribute aPosition to each of the square's vertices.
    gl.aPosition = gl.getAttribLocation(program, "aPosition");
    gl.enableVertexAttribArray(gl.aPosition);
    gl.vertexAttribPointer(gl.aPosition, 3, gl.FLOAT, false, 0, 0);

    // backBuffer stuff
    function createTarget() {
      var target = {
        texture: gl.createTexture(),
        framebuffer: gl.createFramebuffer()
      };
      // set up framebuffer
      gl.bindTexture(gl.TEXTURE_2D, target.texture);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, WIDTH, HEIGHT, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);

      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

      gl.bindFramebuffer(gl.FRAMEBUFFER, target.framebuffer);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, target.texture, 0);

      // clean up
      gl.bindTexture(gl.TEXTURE_2D, null);
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);

      return target;
    }

    state.fb[0] = createTarget();
    state.fb[1] = createTarget();

    state.textures = [];
    function createTexture(image) {
      var texture = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, texture);

      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
      state.textures.push(texture);
    }

    var img = new Image();
    img.src = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFQAAABpAgMAAADZ4ewhAAAADFBMVEVlLWcjHyD///9aukdNbQb8AAAAAXRSTlMAQObYZgAAAAFiS0dEAIgFHUgAAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfgBBsTBjDG601/AAAAbUlEQVRIx2MIxQYYBoVo2Cp0MHXQiC5FcWvUIBJd//9q6P+/ofFAapCJDt4wWxpa/x/s3v//B53o4A2zwSo6WEuNQVvSQgEad1CIMmK41mFwiILprFXgRAilYOE78KKgnLBqJYwKHUSigzMHAADhlJM2vqJTOQAAAABJRU5ErkJggg==";
    createTexture(img);

    // remember the address within the fragment shader of each of my uniforms variables
    gl.time = gl.getUniformLocation(program, "time");
    gl.mouse = gl.getUniformLocation(program, "mouse");
    gl.audio = gl.getUniformLocation(program, "audio");
    gl.resolution = gl.getUniformLocation(program, "resolution");
    gl.backBuffer = gl.getUniformLocation(program, "backBuffer");

    gl.channel1 = gl.getUniformLocation(program, "channel1");

    this.draw();

    if (this.props.animation) {
      if (state.animationFrameRequest === null) {
        //INVARIANT: afr is non-null if we are animating.

        state.animationFrameRequest = requestAnimationFrame(this.animate);
      }
    }
  },
  draw: function draw() {
    var gl = this.gl;
    var state = this.state_;

    gl.uniform1f(gl.time, new Date().getTime() / 1000 - state.time0);
    gl.uniform2f(gl.mouse, state.mouse.x, state.mouse.y);
    gl.uniform2f(gl.resolution, this.props.width, this.props.height);
    gl.uniform4f(gl.audio, state.audio.low, state.audio.mid, state.audio.upper, state.audio.high);

    for (var i = 1; i < state.textures.length + 1; i++) {
      gl.uniform1i(gl.channel1, i);
      gl.activeTexture(gl.TEXTURE0 + i);
      gl.bindTexture(gl.TEXTURE_2D, state.textures[i - 1]);
    }

    gl.uniform1i(gl.backBuffer, 0); // Do I need to check for null?
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, state.fb[state.bit].texture);
    state.bit = (state.bit + 1) % 2;
    gl.bindFramebuffer(gl.FRAMEBUFFER, state.fb[state.bit].framebuffer);
    gl.clear(gl.COLOR_BUFFER_BIT);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, state.fb[state.bit].texture);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.clear(gl.COLOR_BUFFER_BIT);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
  },
  animate: function animate() {
    this.draw();
    this.state_.animationFrameRequest = requestAnimationFrame(this.animate);
  },
  componentDidUpdate: function componentDidUpdate() {
    if (!this.props.animation) {
      cancelAnimationFrame(this.state_.animationFrameRequest);
      this.state_.animationFrameRequest = null;
    }

    this.loadProgram();
  },
  shouldComponentUpdate: function shouldComponentUpdate(nextProps, nextState) {
    if (nextProps.fsSource !== this.props.fsSource) {
      return true;
    }
    return false;
  },
  componentWillUnmount: function componentWillUnmount() {
    cancelAnimationFrame(this.state_.animationFrameRequest);
    /* Audio.removeCallback(this.state_.audioCallback);*/
  },
  render: function render() {
    var _this = this;

    return React.createElement("canvas", { ref: function ref(_ref) {
        return _this.canvas = _ref;
      },
      className: "program",
      width: this.props.width,
      height: this.props.height });
  }
});

exports.fromReact = Pux.fromReact(Program);

