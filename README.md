Slumber in a cell: Honeycomb used by honey bees for food, brood, heating… and sleeping

ABSTRACT: Sleep appears to play an important role in the lives of honey bees, but to understand how and why, it is essential to accurately identify sleep, and to know when and where it occurs. Viewing normally obscured honey bees in their nests would be necessary to calculate the total quantity and quality of sleep and sleep’s relevance to the health and dynamics of a honey bee and its colony. Western honey bees spend much of their time inside cells, and are visible only by the tips of their abdomens when viewed through the walls of an observation hive, or on frames pulled from a typical beehive. Prior studies have suggested that bees spend some of their time inside cells resting or sleeping, with ventilatory movements of the abdomen serving as a telltale sign distinguishing sleep from other behaviors. Bouts of abdominal pulses broken by extended pauses (discontinuous ventilation) in an otherwise relatively immobile bee appears to indicate sleep. Can viewing the tips of abdomens consistently and predictably indicate what is happening with the rest of a bee’s body when inserted deep inside a honeycomb cell? To distinguish a sleeping bee from a bee cleaning cells, eating, or heating developing brood, we used a miniature observation hive with slices of honeycomb turned in cross-section, and filmed the exposed cells with an infrared-sensitive video camera and a thermal camera. Thermal imaging helped us identify heating bees, but simply observing ventilatory movements, as well as larger motions of the posterior tip of a bee’s abdomen was sufficient to noninvasively and predictably distinguish heating and sleeping inside comb cells. Neither behavior is associated with large motions of the abdomen, but heating demands continuous (vs. discontinuous) ventilatory pumping. Among the four behaviors observed inside cells, sleeping constituted 13.5% of observations. Accuracy of identifying sleep when restricted to viewing only the tip of an abdomen was 86.6%, and heating was 73.0%. Monitoring abdominal movements of honey bees offers anyone with a view of honeycomb the ability to more fully monitor when and where behaviors of interest are exhibited in a bustling nest.

This set of R scripts conducts all of the visualization and statistical analysis for the Slumber in a Cell project.

Getting Started
Download the data files associated with this paper, and edit your file paths to execute the code using the files.

Prerequisites, Installations
Install the R programming language from https://www.r-project.org/. Then install R Studio from https://rstudio.com/products/rstudio/download/. You’ll also need certain R packages, which can be installed through R. Packages needed include tidyverse, dplyr, lubridate, ggplot2, reshape2, extrafont, plyr, scales, lmerTest, emmeans, and lme4. Use the following as an example of how to install packages in R:

Install.packages(“tidyverse”)

Running the tests
For Mac, use Cmd + Enter to execute a line of code. For Windows, use Ctrl + Enter. Execute each line or execute it all at once by selecting all of the script and hitting the above keys. Some plots and text files will output as saved files. Indicate your desired path for saving the files, then look for them in that directory on your computer. Other results will be output in the terminal, and should be viewed as they are executed line-by-line.

Built With
•	RStudio

Contributing
When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other method with the owners of this repository before making a change.
Please note we have a code of conduct, please follow it in all your interactions with the project.

Pull Request Process
1.	Ensure any install or build dependencies are removed before the end of the layer when doing a build.
2.	Update the README.md with details of changes to the interface, this includes new environment variables, exposed ports, useful file locations and container parameters.
3.	Increase the version numbers in any examples files and the README.md to the new version that this Pull Request would represent. The versioning scheme we use is SemVer.
4.	You may merge the Pull Request in once you have the sign-off of two other developers, or if you do not have permission to do that, you may request the second reviewer to merge it for you.

Code of Conduct

Our Pledge
In the interest of fostering an open and welcoming environment, we as contributors and maintainers pledge to making participation in our project and our community a harassment-free experience for everyone, regardless of age, body size, disability, ethnicity, gender identity and expression, level of experience, nationality, personal appearance, race, religion, or sexual identity and orientation.

Our Standards
Examples of behavior that contributes to creating a positive environment include:
•	Using welcoming and inclusive language
•	Being respectful of differing viewpoints and experiences
•	Gracefully accepting constructive criticism
•	Focusing on what is best for the community
•	Showing empathy towards other community members
Examples of unacceptable behavior by participants include:
•	The use of sexualized language or imagery and unwelcome sexual attention or advances
•	Trolling, insulting/derogatory comments, and personal or political attacks
•	Public or private harassment
•	Publishing others' private information, such as a physical or electronic address, without explicit permission
•	Other conduct which could reasonably be considered inappropriate in a professional setting

Our Responsibilities
Project maintainers are responsible for clarifying the standards of acceptable behavior and are expected to take appropriate and fair corrective action in response to any instances of unacceptable behavior.
Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct, or to ban temporarily or permanently any contributor for other behaviors that they deem inappropriate, threatening, offensive, or harmful.

Scope
This Code of Conduct applies both within project spaces and in public spaces when an individual is representing the project or its community. Examples of representing a project or community include using an official project e-mail address, posting via an official social media account, or acting as an appointed representative at an online or offline event. Representation of a project may be further defined and clarified by project maintainers.

Enforcement
Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by contacting the project team at [INSERT EMAIL ADDRESS]. All complaints will be reviewed and investigated and will result in a response that is deemed necessary and appropriate to the circumstances. The project team is obligated to maintain confidentiality with regard to the reporter of an incident. Further details of specific enforcement policies may be posted separately.
Project maintainers who do not follow or enforce the Code of Conduct in good faith may face temporary or permanent repercussions as determined by other members of the project's leadership.

Attribution
This Code of Conduct is from PurpleBooth, which adapted it from the Contributor Covenant, version 1.4, available at http://contributor-covenant.org/version/1/4

Authors
•	Kathryn Busby – Wrote and designed scripts and statistical tests – github.com/katbeescience
•	Barrett Klein – Design of figures and statistical tests – pupating.org

License
This project is licensed under the MIT License - see the LICENSE.md file for details

Acknowledgments
Thanks to Dave Reineke, Jeff Oliver, and Keaton Wilson contributed ideas, code, and much consultation to this project.

