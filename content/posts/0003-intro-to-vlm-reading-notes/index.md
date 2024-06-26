+++
title = 'An Introduction to VLM Reading Notes'
date = 2024-05-30T10:00:00-07:00
toc = true
tags = ['vlm']
+++

This is a 76 pages paper. The authors come from Meta, U Toronto, CMU, MIT, NYU, Berkeley, U Marland, KAUST, CIFAR AI.

LLM is popular. Extending to visual domain is naturally a hot topic. It is important for potential applications like *visual assistant* and *generative models*. Challenges lie in the reliability.

## 1. Introduction

LLMs are popular, e.g. LLaMA, ChatGPT. They were limited to text inputs, but now extended to visual input as well. Connecting vision to language is still an open problem, e.g. Most models struggle to

- Understand spatial relationships
- Count without complicated engineering
- Lack of attributes and ordering
- Ignore some part of the input prompt
- Hallucinate

In short, relability is the challenge.

In the introduction, the authors throw at the readers with a list of questions:

- What are VLMs?
- How are VLMs trained?
- How to evaluate VLMs?
- How contrastive methods have changed the field?
- How to leverage masking strategies / generative components?
- What are VLMs using pre-trained backbones?
- Which datasets are appropriate?
- Which data curation strategy should we adopt?
- Do we need to train a text encoder?
- Can we leverage a pre-trained LLM?
- Is a contrastive loss enough for vision understanding?
- Is a generative component key?
- What is grounding?
- What is alignment?
- What are the benchmarks?
- What are the strength and weaknesses of VLM benchmarks
- How to measure bias?

Honestly, these are probably good questions for a job interview (and I don't know everything, that's why I am reading).

## 2. The Families of VLMs

Bridging CV and NLP is hot. This paper focus on models based on transformers. Models can be categorized into 4 training paradigms:

1. Constrastive training: Leverage positive and negative pairs.
   - VLM predicts similar representations for positive examples
   - VLM predicts different representations for negative examples
1. Masking
   - Similar to masking to words in caption
1. Pretrained backbones
   - Leverage LLMs like LLaMA
   - Learn mapping between image encoder and LLM
1. Generative
   - Expensive to train

Early works based on transformers

1. Transformer
2. BERT
3. Visual BERT, ViLBERT

### Contrastive-based VLMs

Better explaination through an Energy-Based Models point of view [LeCun et al., 2006].

A model $E_\theta$ is parameterized by $\theta$, is trained to assign low energy to observed variables and high energy to unobserved ones. If input data is denoted as $x$, then the energy function is $E_\theta(x)$. The corresponding Boltzmann distribution density function is
$$
p_\theta(x) = \frac1{Z_\theta} \mathrm e^{-E_\theta(x)}
$$
where $Z_\theta = \sum_x \mathrm e^{-E_\theta(x)}$ is the partition function (normalization coefficient).

Let the target distribution be $P_D$. The traditional maximum likelihood objective is $$ \mathop{\text{arg min}}\limits_\theta \mathbb E_{x \sim P_D(x)} [-\log P_\theta(x)] $$

> **Note**:
> $P_\theta(x)$ is the same as $p_\theta(x)$, right?
> $P_D$ is actually the probability distribution of data.

The gradient of the objective function is
$$
\frac{\partial \mathbb E_{x \sim P_D(x)} [-\log P_\theta(x)]}{\partial \theta} = \mathbb E_{x^+ \sim P_D(x)} \frac{\partial E_\theta(x^+)}{\partial \theta} - \mathbb E_{x^- \sim P_\theta(x)} \frac{\partial E_\theta(x^-)}{\partial \theta}
$$

> **Why?**

Here $x^-\sim P_\theta(x)$ is most likely intractable. Mitigations can be Markove Chain Monte Carlo (MCMC), Score Matching [Hyvärinen, 2005], Denoising Score Matching [Vincent, 2011], Noise Contrastive Estimation (NCE) [Gutmann and Hyvärinen, 2010].

The intuition behind NCE is sampling from a noise distribution $u' \sim p_n(u')$ might approximate samples from the model distribution well enough, which is of course hard to justify. But it works in recent Self-Supervised Learning literature [Chen et al., 2020].

The original NCE framework:

- Label $C=1$ for real data distribution
- Label $C=0$ for noise distribution
The loss funtion is a cross-entropy: $$
\mathcal L_\text{NCE}(\theta) = -\sum_i \log P(C_i=1|x_i; \theta) - \sum_j \log P(C_j=0| x_j, \theta)
$$ where $x_i$ sampled from data distribution, $x_j (j\ne i)$ sampled from noise distribution.

[Wu et al., 2018] introduced NCE without positive pairs with non-parametric softmax using explicit normalization and a temperature parameter $\tau$.

> **Note**:
> Equation (3) and (2) in [Wu et al. 2018] 
> \begin{align}
> J(\theta) &= -\sum_{i=1}^n \log P(i | f_\theta(x_i)) \\\\
> P(i|\mathbf v) &= \frac{\exp(\mathbf v_i^\top \mathbf v/ \tau)}{\sum_{j=1}^n \exp(\mathbf v_j^\top \mathbf v/ \tau)}
> \end{align}
> Here $\mathbf v = f_\theta (x)$ is the feature of image $x$.

[Oord et al., 2018] Contrastive Predictive Coding (CPC) introduced InfoNCE, keeping non-parametric softmax while using positive pairs:
$$ \mathcal L_\text{infoNCE} = -\sum_{(i,j)\in\mathbb P} \log \left( \frac{\mathrm e^{\text{CoSim}(z_i, z_j) / \tau}}{\sum_k \mathrm e^{\text{CoSim}(z_i, z_k) / \tau}}\right)
$$

> **Note**:
> At a first glance, the formula is slight different from their original paper.

A major draw back is InfoNCE requires a large mini-batch to learn positive and negative samples more effectively.

> **Why?**

- The InfoNCE is adopted by Contrastive Language Image Pretraining (CLIP) [Radford et al., 2021].
- SigLIP [Zhai et al., 2023b] imporved binary InfoNCE to multi-class InfoNCE.
- Latent Language Image Pretraining (LLIP) [Lavoie et al., 2024] improved CLIP by consideration on an image can have multiple captions.

### VLMs with Masking Objectives

Masking can be viewed as a specific form of denoising autoencoder [Vincent et al., 2008]. It's also related to image inpainting [Pathak et al., 2016], or Masked Language Modeling (MLM) introduced in BERT [Devlin et al., 2019]. There are also attempts to do Masked Imaging Modeling (MIM), e.g. Masked AutoEncoder (MAE) [He et al., 2022] or Image with a Joint-Embedding Predictive Architecture (I-JEPA) [Assran et al., 2023]. Recent works that combining vision and language are Foundational Language And Vision Alignment (FLAVA) [Singh et al., 2022], MaskVLM [Kwon et al., 2023].

**Information theoretic view on VLM objectives.** [Federici et al., 2020] first show that VLMs can be understood to solve a rate-distortion problem. A rate-distortion problem takes the following form [Shwartz and LeCun, 2024]
$$
\mathop{\text{arg max}}\limits_{p(z|x)}~~ I(f(X); Z) + \beta \cdot H(X | Z).
$$
To recover masked VLM objective, we bound the above equation;
$$
\mathcal L = \sum_{x\in\mathcal D} \mathbb E_{p(f) p(Z|f(x))} [\log q(z) + \beta \cdot \log q(x|z)].
$$
Here,

- $\log q(z)$ is the entropy bottleneck.
- $I(f(X); Z)$ is bounding the rate, removing superfluous information.
- $H(Z|X)$ is the distortion (not $H(X|Z)$ !).

> **Note:**
>
> - There are some notation that is very unclear, at least to me, e.g., what does it mean to have two probability under an expectation operator?
> - Although interesting, I don't quite understand what does this theory entails.
>
> **Takeaway:**
> "We understand the contrastive loss and auto-encoding loss as implementations of distortions, whereas the rate is mostly determined by the data transformation used."

### Generative-based VLMs

Instead of learning abstraction, generative paradigm considers the generation text and/or image.

- CoCa [Yu et al., 2022b]: learn a complete text encoder and decoder --- enable image captioning.
- Chameleon [Chameleon Team, 2024] and CM3leon[Yu et al., 2023] --- generate both text & image
- Generate image based on text --- Stable Diffusion [Rombach et al., 2022], Imagen [Saharia et al., 2022], Parti [Yu et al., 2022c].
