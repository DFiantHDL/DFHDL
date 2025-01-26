import re

def replace_hdelk(match):
    width = match.group(1) or "100%"  # Default to 100% if width is not specified
    diagram_json = match.group(2)
    diagram_id = f"hdelk-diagram-{hash(diagram_json)}"  # Generate a unique ID
    return f"""
<div id="{diagram_id}" class="hdelk-diagram" style="width: {width};"></div>
<script type="text/javascript">
    document.addEventListener("DOMContentLoaded", function() {{
        var diagram = {diagram_json};
        hdelk.layout(diagram, "{diagram_id}");
    }});
</script>
"""

def on_page_markdown(markdown, **kwargs):
    hdelk_block_pattern = re.compile(r'```hdelk(?:\s+width=([\d.]+%))?\n(.*?)\n```', re.DOTALL)
    new_markdown = re.sub(hdelk_block_pattern, replace_hdelk, markdown)
    return new_markdown
