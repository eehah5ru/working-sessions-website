(function (self) {
  const ITERATION_DELAY = 200;

  var inTOCAdjusting = false;

  //
  //
  // local storage routines
  //
  //
  var withStorage = function (func, rescueFunc) {
    if(!_.isUndefined(Storage)) {
      return func();
    } else {
      console.log("storage is not supported");
      return rescueFunc();
    }
  };

  var saveTOCFontSize = function (fontSize) {
    return withStorage(
      function () {
        sessionStorage.tocFontSize = fontSize;
      },
      function () {
        return null;
      }
    );
  };

  var isTOCFontSizeSaved = function () {
    return withStorage(
      function () {
        // console.log("isTOCFontSizeSaved: ",  sessionStorage.tocFontSize, typeof(sessionStorage.tocFontSize), _.isString(sessionStorage.tocFontSize));
        return _.isString(sessionStorage.tocFontSize) || _.isNumber(sessionStorage.tocFontSize);
      },
      function () {
        return false;
      }
    );
  };

  var forgetTOCFontSize = function () {
    return withStorage(
      function () {
        sessionStorage.removeItem("tocFontSize");
      },
      function () {}
    );
  };

  var getTOCFontSize = function () {
    return withStorage(
      function () {
        return sessionStorage.tocFontSize;
      },
      function () {
        console.log("getting not saved tocFontSize");
        return null;
      }
    );
  };

  //
  //
  // UI routines
  //
  //
  var isMobile = function () {
    var md = new MobileDetect(window.navigator.userAgent);
    return md.mobile() || md.phone() || md.tablet();
  };

  //
  //
  // adjusting
  //
  //

  var hideTOC = function () {
    $(".toc-wrapper").addClass("hidden");
  };

  var showTOC = function () {
    $(".toc-wrapper").removeClass("hidden");
    // alert("toc is visible now");
  };

  var hasTOCOnPage = function () {
    return $(".toc-wrapper").length;
  };


  var getTOCWrapperHeight = function () {
    return $(".toc-wrapper").height();
  };

  var getTOCHeight = function () {
    return $(".toc-wrapper .toc").height();
  };

  var setTOCItemHeight = function (fontSize) {
    $(".toc-wrapper .toc li").css({"font-size": fontSize + "vw"});
  };

  var checkFontSize = function (fontSize, callback) {
    setTOCItemHeight(fontSize);

    callback(null, getTOCHeight() < getTOCWrapperHeight());
  };


  var adjustToc = function (minFontSize, maxFontSize) {
    var delta = maxFontSize - minFontSize;
    var fontSize = minFontSize + delta / 2.0;

    // steps are to small to produce significant changes
    if (delta < 1.0) {
      console.log("end of adjusting ", minFontSize, maxFontSize);
      setTOCItemHeight(minFontSize);
      saveTOCFontSize(minFontSize);
      inTOCAdjusting = false;
      return;
    }

    d3_queue.queue()
      .defer(checkFontSize, fontSize)
      .await(function (error, isOk) {
        if (error) {
          console.log("got error while adjusting TOC font: ", error);
          return;
        }

        if (isOk) {
          setTimeout(_.partial(adjustToc, fontSize, maxFontSize), ITERATION_DELAY);
        }
        else {
          setTimeout(_.partial(adjustToc, minFontSize, fontSize), ITERATION_DELAY);
        }
        return;
      });

  };

  var adjustTocHandler = function () {
    if (!hasTOCOnPage()) {
      return;
    }

    if (inTOCAdjusting) {
      return;
    }

    forgetTOCFontSize();
    inTOCAdjusting = true;
    hideTOC();
    setTOCItemHeight(100);
    showTOC();
    setTimeout(_.partial(adjustToc, 0, 100), 1000);
  };

  //
  //
  // event handlers
  //
  //
  var onWindowSizeChanged = function () {
    if (isMobile()) {
      return;
    }
    // console.log("changed window size");
    // alert("changed window size");
    adjustTocHandler();
  };

  var onOrientationChanged = function () {
    if (!isMobile()) {
      return;
    }

    adjustTocHandler();
  };

  //
  //
  // init
  //
  //
  $(document).ready(function () {
    $(document).foundation();
    setTimeout(function () {
      if (!hasTOCOnPage()) {
        return;
      }

      if (inTOCAdjusting) {
        return;
      }

      if (isTOCFontSizeSaved()) {
        setTOCItemHeight(getTOCFontSize());
        showTOC();
        return;
      }
      console.log("tocFontSize is not saved");

      inTOCAdjusting = true;
      setTOCItemHeight(100);
      showTOC();
      adjustToc(0, 100);
    }, ITERATION_DELAY);
  });

  $(window).on("orientationchange", onOrientationChanged);
  $(window).resize(onWindowSizeChanged);

})(this);
