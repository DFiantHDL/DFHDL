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
