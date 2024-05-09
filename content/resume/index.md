---
title: "Resume"
showtoc: false
---

## Education

- Fudan University, Ph.D in Computational Mathematics, (2008.09 - 2015.01)
- Fudan University, B.S. in Physics,  (2004.09 - 2008.06)

## Experiences

{{< collapse summary="**Chegg Inc.**, Staff Machine Learning Engineer, (2023.05.08 - Now)" >}}

High Quality Question-and-Answer Retriver Model

Quality Score Proximal for Multimodal LLM Training Data Augmentation. Developed an end-to-end solution to increase the data volume for Multimodal Large-Language-Models, without the help of manually quality score.

{{< /collapse >}}

{{< collapse summary="**Amazon.com, Services LLC.**, Research Scientist II, (2021.04.26 - 2023.01.18)" >}}

- On-Site Attribution 2.0 for Amazon Live and Video Shopping Experience
  - Developed, validated, and launched OSA model (a deep LSTM-based causal inference framework) for measuring marketing experiences from Amazon Live and VSE business at multiples granularities across Amazon touchpoints.
  - Facilitated alignment with data scientists and data engineers on understanding of the business logic, initiated analysis regarding large dataset for data ingestion and featurization design.
  - Directed the team to design A/B Testing experiment for model validation and coordinated project progress with economists and stakeholders.
  - Directed the engineering team to productionize and validate the model, prioritized weekly tasks, and generated weekly reports for Live and VSE business.
  - Integrated model outputs into cross-business central financial reporting system, measuring the high impact of large-scale data amounting to ∼60M daily customer video related impressions towards Amazon's $1B daily sales.

- Uncertainty Quantification for OSA Model
  - Crafted research plan on uncertainty quantification for OSA model and influenced research direction on methods balancing theoretical sophistication and practical implementation complexity: including Bootstrap, Monte Carlo Dropout, Bayesian Neural Networks, and $t$-test.
  - Presented as co-speaker after research was highlighted in Amazon Machine Learning Conference (AMLC) Workshop on Uncertainty-Aware Deep Learning.

- On-Site Attribution for Fresh/Food/Fast Expansion
  - Developed cross-functional methodology and implementation to generalize model from measuring on-site purchases towards in-store physical domain.
  - Fine-tuned the model and directed the engineering team to productionize automatic deployment of OSA model.
  - Diligently developed metrology of F3 online campaigns covering $50M daily in-store purchases.
  - Launched and deployed the model, generated weekly reports and measurements for specific seasonal events.

{{< /collapse >}}

{{< collapse summary="**Michelin (China) Investment Co. Ltd.**, Data Scientist and Initiative Leader (2020.02.10 - 2021.01.28)" >}}

- OCR Module on Tire and Wheel Serial Number for Tire Management System
  - Responsible for building a prototype an OCR module for Tire and Wheel Serial Number for Aircraft Tires for AirChina and Beijing AMECO as a building-block for general-purpose tire management system.
  - Successfully developed and launched a Flutter-based frontend on Android Tablet platform, and utilized OCR APIs from multiple platforms (Baidu, Ali, Tencent).
  - Collaborated with engineers and scientists from Thailand, and directed the design of the end-to-end solution.
- Slow Leak Algorithm for Shanghai Metro Public Transportation (Pujiang Line), Largest Chinese Delivery Company (Zhong Tong Express, ZTO), and Leading Chinese Public EV Company (Xiao Peng Motors, Xpeng)
  - Developed and launched a machine-learning-based slow leak detection algorithm for metro, delivery logistic fleets, and EV customers with SmartSeal tires.
  - Created metrics for evaluating the quality of slow leak detection algorithm.
  - Collaborated with engineers and scientists from the US to evaluate performance of several different models, e.g., linear model, exponential model, etc.
- Software Development Consultant: MEMS4 Import
  - Implemented MEMS4 software localization, including evaluation software technical viability of the MEMS4 solution for China, e.g., local data-warehouse hosting, wireless connectivity evaluation, and integration viability with TMS solution from Australia (Klinge's Total Tire Control, TTC), data privacy, and translation.
  - Established testing demo server as a technical proof to support commercial tender.
{{< /collapse >}}

{{< collapse summary="**Signify (China) Investment Co. Ltd. (f.k.a. Philips Lighting)**, Data Scientist (2016.07.18 - 2020.02.06)" >}}

- Project iPole Microwave Sensing Algorithm Training Data
  - Responsible for GUI development (MATLAB), realized semi-auto object detection (focused on automobiles) and established data set for microwave sensing algorithms
  - Leveraged traditional image/video processing techniques (Moving averages, Gaussian Mixture Models for back- ground modeling) to achieve highly eﬀicient foreground detection.
  - Creatively utilized ffmpeg package to video-loading via multi-threads; result in 10x faster performance com- paring to VideoCapture() in OpenCV.
  - Compiled and applied Mask R-CNN (TensorFlow, PyTorch) to target scenarios and evaluate the performance of the state-of-the-art algorithms
  - Leveraged inner parameters of GoPro Lens to perform camera calibration, leading to tangible improvement in mAP.
  - Diligently fine-tuned parameters and strategies for algorithms, delivered a robust system that works under complex weather conditions, weak luminance environment, and vertical FOV (evaluated on internal test data set).
- Philips-Xiaomi Sales-and-Service Management System
  - Refactored and provided a unified Philips-Xiaomi Sales-and-Service platform (Django+MySQL) by integrating 3 originally separated subsystems (sales, analysis, and user-behavior) into 1.
  - Established local sales query cache, reduced latency of the query from the server and the cost of cloud service; the cache provides core data support for future marketing and sales data analysis.
  - Researcher (Best Project Award Winner): Virtualization Applications in China
  - Created web crawler (scrapy, requests, selenium) and data pipeline to grab ideal lighting design cases for professional lighting designer.
- Modeling on Reflection Table for China Road (Collaborative Project with Fudan University)
  - Applied statistical modeling techniques for road surface reflection, reducing relative error of average luminance coeﬀicient $Q_0$, specular factors $S_0$, $S_1$ from about 300% to 5%.
{{< /collapse >}}

{{< collapse summary="**United Imaging Healthcare**, Algorithm Engineer and Team Leader (2015.01.07 - 2016.06.18)" >}}

- Responsible for optimal problem formulations in radiotherapy planning: unconstrained nonlinear program- ming/quadratic programming plus column generation heuristics/nonlinear programming, etc.
- Integrated L-BFGS-B 3.0 into optimization subsystem, removing the dependency of commercial software IBM CPLEX and TOMLAB, reducing the cost $7,790/machine, enabling the optimization for computationally expensive VMAT execution mode.
- Guided team members to develop in-house constrained quadratic programing solver PCQP tailored for radiotherapy problem, replacing application of IBM CPLEX.
- Surveyed the cutting-edge literatures in the field (NVBB algorithm, a method combines dose calculation and treatment optimization), and wrote technical note for future use.
- Responsible for documentation assignments (SIT, SSIT) for subsystem release, drafted and reviewed patents in dose calculation and radiotherapy treatment planning.
{{< /collapse >}}

{{< collapse summary="**University of California, Santa Barbara**, Visiting Scholar (2011.08 - 2012.08)" >}}

Full scholarship sponsored by [Chinese Scholarship Council](https://www.chinesescholarshipcouncil.com/).

{{< /collapse >}}

## Publications

- (with Nixon Li, Nikolaos Chatzipanagiotis, Mohammad Abuzar Hashemi, Piyush Gahlot, Larkin Flodin), Uncertainty Quantification for Neural Networks with Scalable Monte Carlo Dropout, *Amazon Machine Learning Conference*, 2022. (workshop poster).
- (with Mohammad Abuzar Hashemi, Nikolaos Chatzipanagiotis, Nixon Li, Larkin Flodin, Piyush Gahlot, Prag Mishra), Improving Onsite-Content Attribution with Attention, *Amazon Machine Learning Conference*, 2022. (oral presentation).
- (with Weiguo Gao, Carlos J. García-Cervera), High Order Finite Difference Discretization for Composite Grid Hierarchy and Its Applications. *Commun. Comput. Phys.* **18**(5), 2015, pp. 1211-1233.
- (with Weiguo Gao), Inexact Two-Grid Methods for Eigenvalue Problems. J. Comp. Math., **6**, 2015, pp. 557-575.
- (with Hongfang Li, Yin Luo, Weikang Chen, and Wanheng Zhong), The statistical law of the nearly independent particle system (III) --- Canonical distribution function and energy fluctuation formula. *College Physics*, **29**(1), 2010.
- (with Hongfang Li, Weikang Chen, Yin Luo, and Wanheng Zhong), The statistical law of the nearly independent particle system (IV) --- Grand canonical distribution function. *College Physics*, **29**(1), 2010.

## Patents

- (with Gongming Wei and Lei Feng), [Method of and device for commissioning a lighting system](https://patents.google.com/patent/US11044797B2/en), 2019, US11044797B2
- (Qun Gu), [Method and device for calculating emergent beam intensity of radiotherapy](https://patents.google.com/patent/CN105363139A/en), 2016, CN105363139A
- (with Peng Zhang, Panjie Gou), [Radiotherapy treatment planning optimization method and device](https://patents.google.com/patent/CN105561485A/en), 2016, CN105561485A
- (with Youzhong Huang, Zhigang Yuan), [Multichannel film radiation amount measurement method](https://patents.google.com/patent/CN105572713A/en), 2016, CN105572713A
- (with Youzhong Huang, Gui Li), [Image registration method and system](https://patents.google.com/patent/CN105447882A/en), 2016, CN105447882A
- (with Panjie Gou, Jingjie Zhou, Peng Zhang, Gui Li), [Selection method and selection system for dose control points, and radiotherapy planning system](https://patents.google.com/patent/CN105404789A/en), 2016, CN105404789A
