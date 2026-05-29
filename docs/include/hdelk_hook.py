import re
import json
import hashlib
import subprocess
import tempfile
import os
from pyhocon import ConfigFactory

CACHE_DIR = os.path.join('.cache', 'plugin', 'hdelk')
GENERATOR_PATH = os.path.join('docs', 'javascripts', 'hdelk.js')

# The cache key incorporates the generator source so that any change to hdelk.js
# automatically invalidates previously cached SVGs. A deterministic hash (md5) is used
# so the cache is also reused across separate builds (unlike the randomized builtin hash).
try:
    with open(GENERATOR_PATH, 'r', encoding='utf-8') as _gen_file:
        GENERATOR_SRC = _gen_file.read()
except OSError:
    GENERATOR_SRC = ''

def replace_hdelk(match):
    width = match.group(1) or "100%"  # Default to 100% if width is not specified
    diagram_json = json.dumps(ConfigFactory.parse_string(match.group(2)), indent=2)
    cache_key = hashlib.md5((diagram_json + GENERATOR_SRC).encode('utf-8')).hexdigest()
    diagram_id = f"hdelk-diagram-{cache_key}"  # Generate a unique ID
    svg_file_path = os.path.join(CACHE_DIR, f"{diagram_id}.svg")

    # Ensure the cache directory exists
    os.makedirs(CACHE_DIR, exist_ok=True)

    # Check if the cached SVG file already exists
    if not os.path.exists(svg_file_path):
        # Create a temporary file for the JSON input
        with tempfile.NamedTemporaryFile(mode="w", delete=False, suffix='.json') as temp_json_file:
            temp_json_file.write(diagram_json)
            temp_json_file_path = temp_json_file.name
            
        # Run the Node.js script to generate the SVG
        subprocess.run(['node', 'docs/javascripts/hdelk.js', temp_json_file_path, svg_file_path], check=True)
        os.remove(temp_json_file_path)

    # Read the generated SVG content
    with open(svg_file_path, 'r') as f:
        svg_content = f.read()

    return f"""
<div id="{diagram_id}" class="hdelk-diagram" style="width: {width};">
    {svg_content}
</div>
"""

def on_page_markdown(markdown, **kwargs):
    hdelk_block_pattern = re.compile(r'```hdelk(?:\s+width=([\d.]+%))?\n(.*?)\n```', re.DOTALL)
    new_markdown = re.sub(hdelk_block_pattern, replace_hdelk, markdown)
    return new_markdown
