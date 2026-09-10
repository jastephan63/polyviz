/*
 * The scroll driver for pv_story() pages. Dependency-free: one
 * IntersectionObserver per scene watches the prose cards, and whichever
 * card crosses the middle band of the viewport switches the scene's
 * pinned pane to that step's widget (a ~300ms crossfade, defined in
 * pv-story.css). Every widget was rendered up front by htmlwidgets'
 * normal static render - stacked with opacity/visibility so each kept
 * its real box while hidden - which is why activating one is nothing
 * but a class toggle.
 *
 * Where the driver cannot or should not run - no IntersectionObserver,
 * or the reader asked for reduced motion - it stands down without
 * touching the page, and the stylesheet's no-script fallback shows each
 * scene's last widget statically.
 */

(function () {
  "use strict";

  /* Show step `idx` of one scene: its widget in the pane, its card at
     full strength. Everything else fades per the stylesheet. */
  function activate(scene, idx) {
    var widgets = scene.querySelectorAll(".pv-story-widget");
    var steps = scene.querySelectorAll(".pv-story-step");
    var i;
    for (i = 0; i < widgets.length; i++) {
      widgets[i].classList.toggle("pv-active", i === idx);
    }
    for (i = 0; i < steps.length; i++) {
      steps[i].classList.toggle("pv-active", i === idx);
    }
  }

  /* The step to start a scene on: the last card whose top has passed
     the viewport's centre line, so a page reopened mid-scroll wakes up
     showing the right chart. A scene still below the fold has no such
     card and starts on its first step. */
  function currentStep(scene) {
    var steps = scene.querySelectorAll(".pv-story-step");
    var centre = window.innerHeight / 2;
    var current = 0;
    for (var i = 0; i < steps.length; i++) {
      if (steps[i].getBoundingClientRect().top <= centre) {
        current = i;
      }
    }
    return current;
  }

  /* One observer per scene. The rootMargin shrinks the viewport to a
     band around its vertical centre; a card intersecting that band is
     the card being read, and its step takes the pane. Cards are taller
     than the band, so exactly the card under the reader's eye fires. */
  function watch(scene) {
    activate(scene, currentStep(scene));
    var io = new IntersectionObserver(function (entries) {
      for (var i = 0; i < entries.length; i++) {
        if (!entries[i].isIntersecting) {
          continue;
        }
        activate(scene,
          +entries[i].target.getAttribute("data-pv-step") || 0);
      }
    }, { rootMargin: "-45% 0px -45% 0px", threshold: 0 });
    var steps = scene.querySelectorAll(".pv-story-step");
    for (var j = 0; j < steps.length; j++) {
      io.observe(steps[j]);
    }
  }

  function boot() {
    var reduced = window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    if (reduced || !window.IntersectionObserver) {
      return;
    }
    var stories = document.querySelectorAll(".pv-story");
    for (var s = 0; s < stories.length; s++) {
      var scenes = stories[s].querySelectorAll(".pv-story-scene");
      if (!scenes.length) {
        continue;
      }
      /* Announce the driver: from here the stylesheet hides every
         widget except the .pv-active one. Activation below follows in
         the same frame, so no pane shows blank in between. */
      stories[s].classList.add("pv-story-js");
      for (var i = 0; i < scenes.length; i++) {
        watch(scenes[i]);
      }
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})();
