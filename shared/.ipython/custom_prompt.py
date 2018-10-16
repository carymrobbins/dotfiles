# -*- coding: utf-8 -*-
# Adapted from https://dunderdoc.wordpress.com/2018/09/02/how-to-customize-your-ipython-5-prompt/
from __future__ import unicode_literals
import os
from IPython.terminal.prompts import Prompts, Token


class CustomPrompt(Prompts):
    if os.environ.get('IPYTHON_SIMPLE_PROMPT'):
        __CUSTOM_PROMPT = '>>> '
    else:
        __CUSTOM_PROMPT = os.environ.get('IPYTHON_PROMPT', ' ')

    def in_prompt_tokens(self, cli=None):
        return [(Token.Prompt, self.__CUSTOM_PROMPT), ]

    def out_prompt_tokens(self, cli=None):
        return [(Token.Prompt, ''), ]

    def continuation_prompt_tokens(self, cli=None, width=None):
        return [(Token.Prompt, ''), ]
