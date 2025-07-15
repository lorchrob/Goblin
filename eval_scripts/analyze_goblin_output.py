import argparse
import os
import re
from lark import Lark, Transformer, Token

# Lisp-like grammar supporting quoted strings and tokens
lisp_grammar = r"""
    ?start: term
    ?term: "(" CONSTRUCTOR children* ")" -> nested
         | ATOM                         -> atom

    CONSTRUCTOR: /[a-zA-Z0-9\-\_]+/
    children: term

    ATOM: ESCAPED_STRING | /[^\s()"]+/

    %import common.ESCAPED_STRING
    %import common.WS
    %ignore WS
"""

class StripUnderscoreFields(Transformer):
    def nested(self, items):
        constructor_token = items[0]
        if not isinstance(constructor_token, Token):
            return None
        constructor = constructor_token.value
        if constructor.startswith('_'):
            return None
        children = [child for child in items[1:] if child is not None]
        return (constructor, *children)

    def atom(self, items):
        return items[0].value

def parse_with_lark(text):
    parser = Lark(lisp_grammar, start="start")
    tree = parser.parse(text)
    return StripUnderscoreFields().transform(tree)

def main():
    parser = argparse.ArgumentParser(description="Parse CFG outputs and count them.")
    parser.add_argument("--file", required=True, help="Path to the file containing outputs separated by lines with only '$'")
    parser.add_argument("--grammar", required=True, choices=["xml", "csv"], help="Grammar type: xml or csv")

    args = parser.parse_args()

    if not os.path.isfile(args.file):
        print(f"Error: file not found: {args.file}")
        exit(1)

    with open(args.file, "r") as f:
        content = f.read()

    # Split on lines that contain only a '$', with optional whitespace around
    outputs = re.split(r'^\s*\$\s*$', content, flags=re.MULTILINE)
    outputs = [o.strip() for o in outputs if o.strip()]

    print(f"Grammar type: {args.grammar}")
    print(f"Number of outputs: {len(outputs)}")

    for i, output in enumerate(outputs, 1):
        try:
            parsed = parse_with_lark(output)
            print(f"Parsed output {i}: {parsed}")
        except Exception as e:
            print(f"Error parsing output {i}: {e}")

if __name__ == "__main__":
    main()

