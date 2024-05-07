import re
import schemdraw
import schemdraw.elements as elm
from io import StringIO


def replace_schemdraw(match):
    code = match.group(1)
    output = StringIO()
    localspace = {}
    exec(code, globals(), localspace)  # Execute the schemdraw code
    drawing = localspace["drawing"]
    svg = drawing.get_imagedata("svg")
    svg_code = svg.decode()
    # Wrap SVG in a div for centering
    centered_svg = f'<div class="schemdraw-svg">{svg_code}</div>'
    output.write(centered_svg)
    return output.getvalue()


def on_page_markdown(markdown, **kwargs):
    schemdraw_block_pattern = re.compile(r"```schemdraw(.*?)```", re.DOTALL)
    new_markdown = re.sub(schemdraw_block_pattern, replace_schemdraw, markdown)
    return new_markdown
