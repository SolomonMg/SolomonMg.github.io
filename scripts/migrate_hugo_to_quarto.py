#!/usr/bin/env python3
"""Convert the Hugo Academic (Wowchemy) content tree to a Quarto website.

Reads  content/{publication,post,project}/<slug>/index.md
Writes publications/<slug>/index.qmd, blog/<slug>/index.qmd, projects/<slug>/index.qmd
       publications.qmd            (grouped-by-year citation index)
       _includes/featured_pubs.md  ({{< include >}}'d on the landing)

Idempotent: regenerates the three output trees + the two index files on every run.
Pure text transform; no plotting/data conventions apply.
"""
from __future__ import annotations
import re
import shutil
import datetime as dt
from pathlib import Path
import yaml

ROOT = Path(__file__).resolve().parents[1]
# Hugo source was archived under _archive_hugo/ after the initial migration.
SRC = ROOT / "_archive_hugo" / "content"
if not SRC.exists():
    SRC = ROOT / "content"
OUT_PUB = ROOT / "publications"
OUT_BLOG = ROOT / "blog"
OUT_PROJ = ROOT / "projects"
INCLUDES = ROOT / "_includes"

# Wowchemy publication_type code -> label
PUB_TYPE = {
    "0": "Other", "1": "Conference paper", "2": "Journal article",
    "3": "Working paper", "4": "Report", "5": "Book", "6": "Book chapter",
    "7": "Thesis", "8": "Patent",
}

# Folder name -> clean URL slug (only where they differ). Old Hugo URL kept as alias.
PUB_SLUG_OVERRIDE = {"aggarwal20232": "aggarwal-2023-2-million-experiment"}

AUTHOR_BOLD = ("Solomon Messing", "Sol Messing")
TWEET_JS = '<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'


# ---------------------------------------------------------------- parsing
def read_hugo(path: Path):
    text = path.read_text(encoding="utf-8")
    m = re.match(r"^---\n(.*?)\n---\n?(.*)$", text, re.S)
    if not m:
        return {}, text
    fm = yaml.safe_load(m.group(1)) or {}
    return fm, m.group(2).strip()


def clean_quotes(text: str, prose: bool = False) -> str:
    """LaTeX-style ``...'' quotes render as broken inline code in Pandoc.
    Convert a bare `` -> " but NEVER touch a `` that is part of a longer
    backtick run (``` code fences, inline-code spans). Convert closing '' -> "
    only in prose (post/project R code uses '' as an empty-string literal)."""
    text = re.sub(r"(?<!`)``(?!`)", '"', text)
    if prose:
        text = text.replace("''", '"')
    return text


def slugify(s: str) -> str:
    s = s.strip().lower()
    s = s.replace("'", "").replace("’", "")
    s = re.sub(r"[\s_]+", "-", s)
    s = re.sub(r"[^a-z0-9\-]", "", s)
    s = re.sub(r"-{2,}", "-", s).strip("-")
    return s


def year_of(fm: dict) -> int:
    d = fm.get("date")
    if isinstance(d, (dt.date, dt.datetime)):
        return d.year
    if d:
        m = re.match(r"(\d{4})", str(d))
        if m:
            return int(m.group(1))
    return 0


def iso_date(fm: dict) -> str:
    d = fm.get("date")
    if isinstance(d, (dt.date, dt.datetime)):
        return d.strftime("%Y-%m-%d")
    return str(d)[:10] if d else ""


# ---------------------------------------------------------------- rewriting
def rewrite_shortcodes(body: str):
    has_tweet = "{{< x " in body or "{{< x\t" in body
    def tweet(m):
        attrs = m.group(1)
        tid = re.search(r'id="([^"]+)"', attrs)
        usr = re.search(r'user="([^"]+)"', attrs)
        tid = tid.group(1) if tid else ""
        usr = usr.group(1) if usr else "i"
        return (f'<blockquote class="twitter-tweet"><a '
                f'href="https://twitter.com/{usr}/status/{tid}"></a></blockquote>')
    body = re.sub(r"\{\{<\s*x\s+([^>]*?)>\}\}", tweet, body)
    # {{< math >}} ... {{< /math >}}  ->  $$ ... $$
    body = re.sub(r"\{\{<\s*/?math\s*>\}\}", "$$", body)
    if has_tweet and TWEET_JS not in body:
        body = body.rstrip() + "\n\n" + TWEET_JS + "\n"
    return body


def rewrite_links(body: str) -> str:
    # markdown link targets: ](/post/x/) ](post/x/) etc. and pdf paths
    def repl(m):
        target = m.group(1)
        t = target
        t = re.sub(r"^/?post/", "/blog/", t)
        t = re.sub(r"^/?project/", "/projects/", t)
        t = re.sub(r"^/?publication/", "/publications/", t)
        if t.startswith("pdf/"):
            t = "/" + t
        return "](" + t + ")"
    return re.sub(r"\]\(([^)]+)\)", repl, body)


def bold_authors(names: list[str]) -> str:
    out = []
    for n in names:
        n = n.strip()
        out.append(f"**{n}**" if n in AUTHOR_BOLD else n)
    return ", ".join(out)


def fix_pdf_field(url: str) -> str:
    if url and url.startswith("pdf/"):
        return "/" + url
    return url


def dump_fm(d: dict) -> str:
    d = {k: v for k, v in d.items() if v not in (None, "", [], {})}
    return yaml.safe_dump(d, sort_keys=False, allow_unicode=True,
                          default_flow_style=False, width=1000).strip()


def find_image(folder: Path):
    for name in ("featured.png", "featured.jpg", "featured.jpeg"):
        if (folder / name).exists():
            return name
    return None


def link_buttons(fm: dict) -> str:
    btns = []
    if fm.get("url_pdf"):
        btns.append(f"[PDF]({fix_pdf_field(fm['url_pdf'])})" + "{.btn-paper}")
    if fm.get("doi"):
        btns.append(f"[DOI](https://doi.org/{fm['doi']})" + "{.btn-paper}")
    if fm.get("url_code"):
        btns.append(f"[Code]({fm['url_code']})" + "{.btn-paper}")
    if fm.get("url_video"):
        btns.append(f"[Video]({fm['url_video']})" + "{.btn-paper}")
    if fm.get("url_slides"):
        btns.append(f"[Slides]({fm['url_slides']})" + "{.btn-paper}")
    for lk in fm.get("links", []) or []:
        if isinstance(lk, dict) and lk.get("url"):
            btns.append(f"[{lk.get('name','Link')}]({lk['url']})" + "{.btn-paper}")
    return " ".join(btns)


# ---------------------------------------------------------------- publications
def migrate_publications():
    if OUT_PUB.exists():
        shutil.rmtree(OUT_PUB)
    pubs = []
    for folder in sorted((SRC / "publication").iterdir()):
        idx = folder / "index.md"
        if not idx.exists():
            continue
        fm, body = read_hugo(idx)
        slug = PUB_SLUG_OVERRIDE.get(folder.name, folder.name)
        authors = fm.get("authors", []) or []
        venue = (fm.get("publication") or "").strip()
        venue_plain = venue.replace("*", "")
        yr = year_of(fm)
        ptype = PUB_TYPE.get(str((fm.get("publication_types") or ["0"])[0]), "Other")
        featured = bool(fm.get("featured"))

        cats = [ptype]
        if featured:
            cats.append("Featured")

        dest = OUT_PUB / slug
        dest.mkdir(parents=True, exist_ok=True)
        img = find_image(folder)
        if img:
            shutil.copy2(folder / img, dest / img)

        aliases = [f"/publication/{folder.name}/"]
        if slug != folder.name:
            aliases.append(f"/publication/{slug}/")

        out_fm = {
            "title": fm.get("title", "").strip(),
            "date": iso_date(fm),
            "author": authors,
            "categories": cats,
            "description": f"{venue_plain}, {yr}".strip(", "),
            "aliases": aliases,
        }
        if img:
            out_fm["image"] = img

        cite = bold_authors(authors)
        doi_txt = f" doi:[{fm['doi']}](https://doi.org/{fm['doi']})" if fm.get("doi") else ""
        parts = [f"{cite} ({yr}). {fm.get('title','').strip()}."]
        if venue:
            parts.append(f"*{venue_plain}*.")
        body_lines = [" ".join(parts) + doi_txt]
        btns = link_buttons(fm)
        if btns:
            body_lines += ["", btns]
        if fm.get("abstract"):
            body_lines += ["", "## Abstract", "", clean_quotes(fm["abstract"].strip(), prose=True)]
        extra = clean_quotes(rewrite_links(rewrite_shortcodes(body)), prose=True).strip()
        if extra:
            body_lines += ["", "## More", "", extra]

        (dest / "index.qmd").write_text(
            "---\n" + dump_fm(out_fm) + "\n---\n\n" + "\n".join(body_lines) + "\n",
            encoding="utf-8")

        pubs.append({
            "slug": slug, "title": fm.get("title", "").strip(), "year": yr,
            "authors": authors, "venue_plain": venue_plain, "doi": fm.get("doi"),
            "url_pdf": fix_pdf_field(fm.get("url_pdf", "")), "url_code": fm.get("url_code"),
            "featured": featured, "date": iso_date(fm),
        })
    return pubs


def citation_row(p: dict) -> str:
    title = f"[{p['title']}](/publications/{p['slug']}/)"
    authors = bold_authors(p["authors"])
    venue = f"*{p['venue_plain']}*, " if p["venue_plain"] else ""
    links = []
    if p["url_pdf"]:
        links.append(f"[PDF]({p['url_pdf']})" + "{.btn-paper}")
    if p["doi"]:
        links.append(f"[DOI](https://doi.org/{p['doi']})" + "{.btn-paper}")
    if p["url_code"]:
        links.append(f"[Code]({p['url_code']})" + "{.btn-paper}")
    link_line = ("  \n" + " ".join(links)) if links else ""
    return (f"::: {{.pub-item}}\n"
            f"**{title}**  \n"
            f'<span class="pub-authors">{authors}</span>  \n'
            f'<span class="pub-venue">{venue}{p["year"]}</span>.'
            f"{link_line}\n:::")


def write_publications_index(pubs: list[dict]):
    pubs_sorted = sorted(pubs, key=lambda p: (p["date"], p["title"]), reverse=True)
    lines = [
        "---", "title: \"Publications\"",
        "description: \"Peer-reviewed articles, books, and working papers.\"",
        "toc: true", "toc-location: right", "---", "",
        "See also my [Google Scholar profile]"
        "(https://scholar.google.com/citations?user=to63kiAAAAAJ&hl=en).", "",
    ]
    cur = None
    for p in pubs_sorted:
        if p["year"] != cur:
            cur = p["year"]
            lines.append(f'\n<h2 class="pub-year">{cur}</h2>\n')
        lines.append(citation_row(p))
        lines.append("")
    (ROOT / "publications.qmd").write_text("\n".join(lines) + "\n", encoding="utf-8")


def write_featured_include(pubs: list[dict]):
    INCLUDES.mkdir(exist_ok=True)
    feat = [p for p in pubs if p["featured"]]
    feat = sorted(feat, key=lambda p: (p["date"], p["title"]), reverse=True)[:4]
    lines = []
    for p in feat:
        lines.append(citation_row(p))
        lines.append("")
    (INCLUDES / "featured_pubs.md").write_text("\n".join(lines) + "\n", encoding="utf-8")


# ---------------------------------------------------------------- posts
def migrate_posts():
    if OUT_BLOG.exists():
        shutil.rmtree(OUT_BLOG)
    for folder in sorted((SRC / "post").iterdir()):
        idx = folder / "index.md"
        if not idx.exists():
            continue
        fm, body = read_hugo(idx)
        slug = fm.get("slug") or slugify(folder.name)
        dest = OUT_BLOG / slug
        dest.mkdir(parents=True, exist_ok=True)
        img = find_image(folder)
        if img:
            shutil.copy2(folder / img, dest / img)
        # copy any other referenced images in the folder
        for extra_img in folder.glob("*"):
            if extra_img.suffix.lower() in (".png", ".jpg", ".jpeg", ".gif", ".svg") \
                    and extra_img.name != img:
                shutil.copy2(extra_img, dest / extra_img.name)

        cats = list(dict.fromkeys((fm.get("tags") or []) + (fm.get("categories") or [])))
        desc = (fm.get("subtitle") or fm.get("summary") or "").strip()
        out_fm = {
            "title": fm.get("title", "").strip(),
            "date": iso_date(fm),
            "author": fm.get("authors", ["Sol Messing"]),
            "categories": cats,
            "description": desc,
            "aliases": [f"/post/{slug}/"],
        }
        if img:
            out_fm["image"] = img
        new_body = clean_quotes(rewrite_links(rewrite_shortcodes(body)), prose=False)
        (dest / "index.qmd").write_text(
            "---\n" + dump_fm(out_fm) + "\n---\n\n" + new_body.strip() + "\n",
            encoding="utf-8")


# ---------------------------------------------------------------- projects
def migrate_projects():
    """Projects are consolidated into the blog as posts tagged 'Project'.
    Old /project/<folder>/ and /projects/<folder>/ URLs are kept as aliases.
    Runs after migrate_posts(), writing into the existing blog/ tree."""
    for folder in sorted((SRC / "project").iterdir()):
        idx = folder / "index.md"
        if not idx.exists():
            continue
        fm, body = read_hugo(idx)
        slug = slugify(folder.name)
        dest = OUT_BLOG / slug
        dest.mkdir(parents=True, exist_ok=True)
        img = find_image(folder)
        if img:
            shutil.copy2(folder / img, dest / img)
        for extra_img in folder.glob("*"):
            if extra_img.suffix.lower() in (".png", ".jpg", ".jpeg", ".gif", ".svg") \
                    and extra_img.name != img:
                shutil.copy2(extra_img, dest / extra_img.name)

        out_fm = {
            "title": fm.get("title", "").strip(),
            "date": iso_date(fm),
            "author": ["Sol Messing"],
            "description": (fm.get("summary") or "").strip(),
            "aliases": [f"/project/{folder.name}/", f"/projects/{folder.name}/"],
        }
        if img:
            out_fm["image"] = img
        btns = link_buttons(fm)
        new_body = clean_quotes(rewrite_links(rewrite_shortcodes(body)), prose=False).strip()
        body_out = (btns + "\n\n" + new_body) if btns else new_body
        (dest / "index.qmd").write_text(
            "---\n" + dump_fm(out_fm) + "\n---\n\n" + body_out + "\n",
            encoding="utf-8")


def main():
    pubs = migrate_publications()
    write_publications_index(pubs)
    write_featured_include(pubs)
    migrate_posts()
    migrate_projects()
    nf = sum(1 for p in pubs if p["featured"])
    print(f"publications: {len(pubs)} ({nf} featured)")
    print(f"blog posts:   {len(list(OUT_BLOG.glob('*/index.qmd')))} (incl. projects tagged 'Project')")


if __name__ == "__main__":
    main()
