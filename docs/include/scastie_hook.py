import re


# Adds `in-browser.css` style for all `in-browser/` pages
def inject_css_on_specific_pages(page, config, files, **kwargs):
    # Check if the page URL includes the 'in-browser/' directory
    cssPath = "css/in-browser.css"
    if "in-browser/" in page.url:
        # Define the custom CSS to be included
        # Check if there's already extra CSS, append if there is, or create if there isn't
        if "extra_css" in config:
            if cssPath not in config["extra_css"]:
                config["extra_css"].append(cssPath)
        else:
            config["extra_css"] = [cssPath]
    elif "extra_css" in config and cssPath in config["extra_css"]:
        config["extra_css"].remove(cssPath)


def on_page_context(context, page, config, nav):
    inject_css_on_specific_pages(page, config, nav)
    return context


# pymdownx superfences rejects bare `key="value"` options on custom fences whose
# keys aren't whitelisted, but accepts the attr_list-style `{ ... }` form. Rewrite
# ```scastie main="X" → ```scastie { data-main="X" } so scastie.js can read the
# resulting data-main attribute and inject `Compile / run / mainClass := Some("X")`.
SCASTIE_FENCE_HEADER = re.compile(
    r'^(?P<prefix>[ \t]*)```scastie(?P<args>[^\n]*)$', re.MULTILINE
)
SCASTIE_MAIN_ARG = re.compile(r'\bmain\s*=\s*"(?P<value>[^"]+)"')


def _rewrite_scastie_header(match):
    args = match.group('args')
    main_match = SCASTIE_MAIN_ARG.search(args)
    if not main_match:
        return match.group(0)
    return '{prefix}```scastie {{ data-main="{value}" }}'.format(
        prefix=match.group('prefix'),
        value=main_match.group('value'),
    )


def on_page_markdown(markdown, **kwargs):
    return SCASTIE_FENCE_HEADER.sub(_rewrite_scastie_header, markdown)
