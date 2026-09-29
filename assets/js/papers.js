// Progressive enhancement for the research paper list.
//
// On desktop, the abstract dropdown opens on hover (pure CSS). Touch screens
// have no hover, so here we let a tap/click anywhere in a paper's box toggle
// its dropdown. Opening one box collapses any other that's pinned open
// (accordion behaviour). Clicks on links inside a box navigate normally.
(function () {
  "use strict";

  var papers = Array.prototype.slice.call(
    document.querySelectorAll(".paper")
  );

  function setOpen(paper, open) {
    paper.classList.toggle("is-open", open);
    var toggle = paper.querySelector(".paper__toggle");
    if (toggle) {
      toggle.setAttribute("aria-expanded", open ? "true" : "false");
      toggle.setAttribute(
        "aria-label",
        open ? "Hide abstract" : "Show abstract"
      );
    }
  }

  function collapseOthers(current) {
    papers.forEach(function (other) {
      if (other !== current) {
        setOpen(other, false);
        other.classList.remove("is-collapsed");
      }
    });
  }

  papers.forEach(function (paper) {
    paper.addEventListener("click", function (event) {
      // Let clicks on links (title/PDF, arXiv, DOI, related posts) behave normally.
      if (event.target.closest("a")) return;

      var willOpen = !paper.classList.contains("is-open");
      collapseOthers(paper);

      if (willOpen) {
        paper.classList.remove("is-collapsed");
        setOpen(paper, true);
      } else {
        // Collapse immediately, even if the pointer is still hovering: the
        // is-collapsed class overrides the CSS hover reveal.
        setOpen(paper, false);
        paper.classList.add("is-collapsed");
      }
    });

    // Mousing over a paper collapses any other that's pinned open, so the hover
    // preview never appears alongside an already-expanded box.
    paper.addEventListener("mouseenter", function () {
      collapseOthers(paper);
    });

    // Once the pointer leaves, drop the hover-suppression so a future hover
    // previews the abstract again.
    paper.addEventListener("mouseleave", function () {
      paper.classList.remove("is-collapsed");
    });
  });
})();
