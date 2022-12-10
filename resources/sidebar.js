const defaultNamespace = document.querySelector("#Prelude")
defaultNamespace.classList.add("active")
const activeNamespaces = new Set()

document.querySelectorAll('.accordion-collapse').forEach(el => {
  let ns = document.querySelector("#" + CSS.escape(el.getAttribute("data-ns")))
  el.addEventListener("show.bs.collapse", _ => makeNsActive(ns))
  el.addEventListener("hide.bs.collapse", _ => makeNsInactive(ns))
})

function makeNsActive(ns) {
  activeNamespaces.add(ns)
  if(!activeNamespaces.has(defaultNamespace))
    defaultNamespace.classList.remove("active")
  ns.classList.add("active")
  ns.scrollIntoView()
}

function makeNsInactive(ns) {
  activeNamespaces.delete(ns)
  ns.classList.remove("active")
  if(activeNamespaces.size == 0)
    defaultNamespace.classList.add("active")
}

function showDoc(id) {
  document.querySelector("#" + CSS.escape(id)).scrollIntoView({behavior: "smooth", block: "center"})
}
