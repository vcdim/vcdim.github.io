+++
title = 'Idefics Code Reading Notes'
date = 2024-04-24T21:24:11-07:00
toc = true
+++

## Background

IDEFICS is a **M**ultimodal **L**arge **L**anguage **M**odel (MLLM) or **V**ision-**L**anguage **M**odel (VLM) published first on [arXiv](https://arxiv.org/abs/2306.16527) on June 21, 2023, and then on [HuggingFace](https://huggingface.co/blog/idefics) on August 22, 2023. I had a chance to use this model to do some experiment but I have never had investigate the package in depth.

Fortunately, the model is an open source code on the `transformers` repo on [GitHub](https://github.com/huggingface/transformers/tree/main/src/transformers/models/idefics), which leaves me with some clues to learn more.

## Structure

In the `idefics` folder, there are 7 files, including `__init__.py`. We have `configuration_idefics.py`, `image_processing_idefics.py`, `modeling_idefics.py`, `perceiver.py`, `processing_idefics.py`, and `vision.py`. Reading from name, seems that `modeling_idefics.py` is probably a good starting point.

### Modeling

#### Imports

Opening `modeling_idefics.py`, we have a pretty large file that contains 1588 lines of code. I am learning some very basic things from this code.

> **Learning Point 1** - `dataclasses`

First, the first clause is

```python
from dataclasses import dataclass
```

Here, `dataclasses` is a Python Standard Library which I didn't use frequently before. It is a `decorator` which is conceptually wrapper (of a function). `decorator` can also be put in front of a class, which makes writing class much easier. On the [documentation](https://docs.python.org/3/library/dataclasses.html) of `dataclasses`, there is a very good illustrative example. For the code below,

```python
from dataclasses import dataclass

@dataclass
class InventoryItem:
    """Class for keeping track of an item in inventory."""
    name: str
    unit_price: float
    quantity_on_hand: int = 0

    def total_cost(self) -> float:
        return self.unit_price * self.quantity_on_hand
```

`@dataclass` will add an initial function `__init__()` that looks like this:

```python
def __init__(self, name: str, unit_price: float, quantity_on_hand: int = 0):
    self.name = name
    self.unit_price = unit_price
    self.quantity_on_hand = quantity_on_hand
```

> **Learning Point 2** - `typing`

The second clause:

```python
from typing import Any, Dict, List, Optional, Tuple, Union
```

is also something I haven't been using very often. It enables a more strict way of programming Python.

Then, there are 5 lines of PyTorch imports, pretty commonly seen.

> **Learning Point 3** - relative import

After that, there are some relative imports from this package:

```python
from ... import PreTrainedModel
from ...activations import ACT2FN
from ...modeling_attn_mask_utils import _prepare_4d_causal_attention_mask_for_sdpa
from ...modeling_outputs import ModelOutput
from ...modeling_utils import PretrainedConfig
from ...pytorch_utils import ALL_LAYERNORM_LAYERS
from ...utils import (
    add_start_docstrings,
    add_start_docstrings_to_model_forward,
    logging,
    replace_return_docstrings,
)
from .configuration_idefics import IdeficsConfig
from .perceiver import IdeficsPerceiverResampler
from .vision import IdeficsVisionTransformer
```

One dot is current directory (`idefics`). Two dots is parent directory (`models`). Three docs is grandparent directory (`transformers`).

> **Learning Point 4** --- A dictionary with function with arguments

`ACT2FN` is defined in the `activations.py` under `transformers` folder as

```python
ACT2FN = ClassInstantier(ACT2CLS)
```

where `ACT2CLS` is a dictionary,

```python
ACT2CLS = {
    "gelu": GELUActivation,
    "gelu_10": (ClippedGELUActivation, {"min": -10, "max": 10}),
    "gelu_fast": FastGELUActivation,
    "gelu_new": NewGELUActivation,
    "gelu_python": (GELUActivation, {"use_gelu_python": True}),
    "gelu_pytorch_tanh": PytorchGELUTanh,
    "gelu_accurate": AccurateGELUActivation,
    "laplace": LaplaceActivation,
    "leaky_relu": nn.LeakyReLU,
    "linear": LinearActivation,
    "mish": MishActivation,
    "quick_gelu": QuickGELUActivation,
    "relu": nn.ReLU,
    "relu2": ReLUSquaredActivation,
    "relu6": nn.ReLU6,
    "sigmoid": nn.Sigmoid,
    "silu": nn.SiLU,
    "swish": nn.SiLU,
    "tanh": nn.Tanh,
}
```

whose keys are shorthands of activation functions classes (PyTorch Module), and values are the activation function class themselves. The `ClassInstantier` is defined just above

```python
class ClassInstantier(OrderedDict):
    def __getitem__(self, key):
        content = super().__getitem__(key)
        cls, kwargs = content if isinstance(content, tuple) else (content, {})
        return cls(**kwargs)
```

which is a subclass that inherits `OrderedDict`, where `__getitem__` is getting override. Effectively, when one call

```python
ACT2FN["relu"]
```

one will get a `nn.ReLU` module.

In the `modeling_attn_mask_utils` there is a function `_prepare_4d_causal_attention_mask_for_sdpa` getting imported. Here, SDPA means **S**caled **D**ot **P**roduct **A**ttention, which is a critical layer in transformer models. The code is a bit daunting at first look, but I can at least get a sense what is going on at high level.

This function takes a attention mask, input shape, inputs embeddings, past key value length, sliding window as inputs. And outputs a expanded 4d mask. At first, an attention mask converter is getting created. The new key value length is the current input shape plus the past key value length. Then a boolean variable `is_tracing` is defined, which seems magical. Then, a ignore causal mask is getting created. If is not None, then return None. If attention mask is None, then it's some how generated. Otherwise, attention mask is not None, and there are some other steps to generate the expanded 4d mask.

I know the above paragraph is a bit hard to get because I haven't fully got it. But there are some other questions in my mind.

> **Learning Point 5** --- What is `Union`?

Union is another name for "or" or "$\cup$" that is used for construct a supertype, e.g. `var: str | None` accepts a variable that is either string or `None`.

> **Learning Point 6** --- `torch`

**What is `torch.finfo`?** `torch.finfo` is similar to `numpy.finfo`, which gives the information of float type. There is a related routine called `iinfo` which gives the information for the integer type. By information, we mean, min, max, precision, etc.

**What is `torch.jit.is_tracing`? What is `torch.fx.Proxy`? What is `torch._dynamo`?** These are all related with computational graph, compiler of torch, which is aiming at high performance computing.

Continue reading, there are some other classes getting imported, e.g. `ModelOutput`, `PretrainedConfig`, `ALL_LAYERNORM_LAYERS`, 3 routines related to docstrings, and 1 with logging.
 
There are 3 more classes coming from the current package: `IdeficsConfig`, `IdeficsPerceiverResampler`, and `IdeficsVisionTransformer`. There is one more constant that is getting imported, that is `IDEFICS_PRETRAINED_MODEL_ARCHIVE_LIST`.

Logging is set to `logger` global variable, and `_CONFIG_FOR_DOC` is a string called `"IdeficsConfig"`.

#### `IdeficsBaseModelOutputWithPast`

This is the first class of this file. It contains a past key/values to speed up sequential decoding. It is a subclass of `ModelOutput`, where there are 5 additional variables:

- `last_hidden_state` --- float tensor
- `past_key_values` --- ((float tensor, float tensor), (float tensor, float tensor), ...)
- `hidden_states` --- (float tensor, float tensor, ...)
- `attentions` --- (float tensor, float tensor, ...)
- `image_hidden_states`  --- (float tensor, float tensor, ...)

#### `IdeficsCausalLMOutputWithPast`

Very similar to the previous one, but with 6 additional variables:

- `loss` 
- `logits`
- `past_key_values`
- `hidden_states`
- `attentions`
- `image_hidden_states`

So no `last_hidden_state`, but with 2 additional variables: `loss` and `logits`.

#### `expand_inputs_for_generation`

## Remaining Questions

About `AttentionMaskConverter`

- What does `_ignore_causal_mask_sdpa` do?
- What does `to_causal_4d` do?
- What does `to_4d` do?
- What does `_unmask_unattended` do?


## Further reading

- [Illustrated Transformer by Jay Alammar](https://jalammar.github.io/illustrated-transformer/)
- [Tensorflow Tutorial on Transformer](https://www.tensorflow.org/text/tutorials/transformer)
