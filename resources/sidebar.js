const defaultNamespace = document.querySelector("#Prelude")
const activeNamespaces = new Set()

const namespaces = new Map()

window.addEventListener("load", _ => {
  let hash = decodeURIComponent(window.location.hash)
  if (hash.length > 0) {
    let [_, id, ns] = hash.match(/#((.*):.*:.*)/)
    makeNsActive(document.querySelector("#" + CSS.escape(ns)))
    showDoc(id)
  } else {
    defaultNamespace.classList.add("active")
  }
});

document.querySelectorAll('.accordion-item').forEach(item => {
  let ns = document.querySelector("#" + CSS.escape(item.getAttribute("data-ns")))
  let collapse = item.querySelector(".accordion-collapse")
  collapse.addEventListener("show.bs.collapse", _ => makeNsActive(ns))
  collapse.addEventListener("hide.bs.collapse", _ => makeNsInactive(ns))
  let links = []
  item.querySelectorAll(".sidebar-link").forEach(link => {
    link.onclick = _ => showDoc(link.getAttribute("data-link"))
    links.push([link, link.getAttribute("data-name")])
  })
  namespaces.set(item, [ns, links])
})

function makeNsActive(ns) {
  activeNamespaces.add(ns)
  if (!activeNamespaces.has(defaultNamespace))
    defaultNamespace.classList.remove("active")
  ns.classList.add("active")
  ns.scrollIntoView()
}

function makeNsInactive(ns) {
  activeNamespaces.delete(ns)
  ns.classList.remove("active")
  if (activeNamespaces.size == 0) {
    defaultNamespace.classList.add("active")
    defaultNamespace.scrollIntoView()
  }
}

var activeDoc = null

function showDoc(id) {
  let el = document.querySelector("#" + CSS.escape(id))
  el.scrollIntoView({behavior: "smooth", block: "center"})
  if (activeDoc)
    activeDoc.classList.remove("active")
  activeDoc = el
  el.classList.add("active")
}

function onSearch(el) {
  let searchTerm = el.value
  if (searchTerm.length > 0) {
    namespaces.forEach(([ns, links], item) => {
      item.querySelector(".accordion-collapse").style.display = "block"
      item.querySelector(".accordion-button").classList.add("disabled")
      var found = false
      links.forEach(([l, n]) => {
        if (n.includes(searchTerm)) {
          found = true
          l.style.display = "block"
        } else {
          l.style.display = "none"
        }
      })
      item.style.display = found ? "block" : "none"
      ns.style.display = found ? "block" : "none"
    })
  } else {
    document.querySelectorAll(".accordion-collapse").forEach(el => el.removeAttribute("style"))
    document.querySelectorAll(".accordion-button").forEach(el => el.classList.remove("disabled"))
    document.querySelectorAll(".accordion-item").forEach(el => el.removeAttribute("style"))
    document.querySelectorAll(".sidebar-link").forEach(el => el.removeAttribute("style"))
    document.querySelectorAll(".nspace").forEach(el => el.removeAttribute("style"))
  }
}
