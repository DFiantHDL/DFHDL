# Adds `in-browser.css` style for all `in-browser/` pages
def inject_css_on_specific_pages(page, config, files, **kwargs):
    # Check if the page URL includes the 'in-browser/' directory
    if "in-browser/" in page.url:
        # Define the custom CSS to be included
        custom_css_url = config["site_url"] + "css/in-browser.css"
        # Check if there's already extra CSS, append if there is, or create if there isn't
        if "extra_css" in config:
            print(config["extra_css"])
            config["extra_css"].append(custom_css_url)
        else:
            config["extra_css"] = [custom_css_url]


def on_page_context(context, page, config, nav):
    inject_css_on_specific_pages(page, config, nav)
    return context
