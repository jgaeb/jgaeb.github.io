function splitFootnotes() {
  ps = document.getElementsByTagName("p");

  /* For each paragraph... */
  for (let p of ps) {
    let prev = null;
    let curr = null;

    /* ... check if any footnotes are adjacent */
    for (let node of p.childNodes) {
      prev = curr;
      curr = node;

      if (prev != null && prev.tagName == "SUP" && curr.tagName == "SUP") {
        curr.classList.add("multiple-fn");
      };
    };
  };
};

document.addEventListener("DOMContentLoaded", () => {
  /* Add commas between adjacent footnotes. */
  splitFootnotes();
});
