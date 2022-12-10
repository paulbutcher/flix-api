const buttons = document.querySelectorAll('.accordion-collapse')
buttons.forEach(el => {
  el.addEventListener("show.bs.collapse", _ => {
    document.querySelector("#" + CSS.escape(el.getAttribute("data-ns"))).scrollIntoView()
  })
})

function showDoc(id) {
  document.querySelector("#" + CSS.escape(id)).scrollIntoView({behavior: "smooth", block: "center"})
}
