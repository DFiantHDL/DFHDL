# Initial Setup {#getting-started}

DFHDL is a domain specific language (DSL) library written in the [Scala programming language](https://www.scala-lang.org){target="_blank"} (Scala 3.7.0-RC4), and as such it lets you utilize the entire Scala ecosystem, including IDEs, various tools, and other libraries. 

Is your system already fit for Scala development? [Jump to the DFHDL hello-world section][hello-world]

## Installing Scala and Other Dependencies

We recommend directly installing Scala 3.7.0-RC4 (no need to install either [Coursier](https://get-coursier.io/){target="_blank"}, [Scala CLI](https://scala-cli.virtuslab.org/){target="_blank"}, or [sbt](https://www.scala-sbt.org/){target="_blank"}):

<div class="grid cards" markdown>

-   :fontawesome-brands-windows:{ .lg .middle } __Windows Instructions__

    ---

    /// tab | Using [Chocolatey](https://community.chocolatey.org/)
    Run the following in Windows command or powershell:

    ```{.cmd .copy linenums="0"}
    choco install scala --version=3.7.0-RC4 
    ```
    ///

-   :fontawesome-brands-linux:{ .lg .middle } __Linux Instructions__

    ---

    /// tab | Using [SDKMAN!](https://sdkman.io/)
    Run the following in your shell:

    ```{.sh-session .copy linenums="0"}
    sdk install scala 3.7.0-RC4 
    ```
    ///

-   :fontawesome-brands-apple:{ .lg .middle } __macOS Instructions__

    ---

    /// tab | Using [Homebrew](https://brew.sh/)
    Run the following in your shell:

    ```{.sh-session .copy linenums="0"}
    brew update
    brew install scala
    ```
    ///
    
-   :material-help:{ .lg .middle } __Other OS/Instructions__

    ---

    If for some reason the instructions above don't work for you, you can try to install Scala via [Coursier](https://get-coursier.io/){target="_blank"} using the information below.

</div>

/// details | Installing Scala via [Coursier](https://get-coursier.io/){target="_blank"}
    type: info
We recommend directly installing Scala, as instructed above. However, if you are experiencing issues, you can try installing Scala via Coursier, as follows:

<div class="grid cards" markdown>

-   :fontawesome-brands-windows:{ .lg .middle } __Windows Instructions__

    ---

    /// tab | Manual
    1. Download the [installer zip file](https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-win32.zip).

    2. Open the zip.

    3. Double click the `cs-x86_64-pc-win32.exe` executable to extract and run Coursier setup.
    ///

    /// tab | CMD
    Run the following in Windows command:

    ```{.cmd .copy linenums="0"}
    curl -fLo cs-x86_64-pc-win32.zip https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-win32.zip
    tar -xf cs-x86_64-pc-win32.zip
    move cs-x86_64-pc-win32.exe cs.exe
    .\cs setup
    ```
    ///

    /// tab | Powershell
    Run the following in Windows Powershell:

    ```{.powershell .copy linenums="0"}
    Invoke-WebRequest -Uri "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-win32.zip" -OutFile "cs-x86_64-pc-win32.zip"
    Expand-Archive -Path "cs-x86_64-pc-win32.zip"
    Rename-Item -Path "cs-x86_64-pc-win32.exe" -NewName "cs.exe"
    Remove-Item -Path "cs-x86_64-pc-win32.zip"
    .\cs setup
    ```
    ///


-   :fontawesome-brands-linux:{ .lg .middle } __Linux Instructions__

    ---

    /// tab | x86-64 (aka AMD64)
    Run the following in your shell:

    ```{.sh-session .copy linenums="0"}
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
    chmod +x cs
    ./cs setup
    ```
    ///

    /// tab | ARM64

    Run the following in your shell:

    ```{.sh-session .copy linenums="0"}
    curl -fL "https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-pc-linux.gz" | gzip -d > cs
    chmod +x cs
    ./cs setup
    ```
    ///

    /// tab | Other linux
    [:octicons-arrow-right-24: Goto Coursier's website](https://get-coursier.io/docs/cli-installation){target="_blank"}
    ///


-   :fontawesome-brands-apple:{ .lg .middle } __macOS Instructions__

    ---

    /// tab | via Brew
    Run the following in your shell:

    ```{.sh-session .copy linenums="0"}
    brew install coursier/formulas/coursier
    cs setup
    ```
    ///

    /// tab | aarch64 (M1,M2,...)
    Run the following in your shell:

    ```{.sh-session .copy linenums="0"}
    curl -fL https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-apple-darwin.gz | gzip -d > cs
    chmod +x cs
    ./cs setup
    ```
    ///

    /// tab | x86-64
    Run the following in your shell:

    ```{.sh-session .copy linenums="0"}
    curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-apple-darwin.gz | gzip -d > cs
    chmod +x cs
    ./cs setup
    ```
    ///

-   :material-help:{ .lg .middle } __Other OS/Instructions__

    ---

    For other OS or instructions please consult the Coursier website.

    [:octicons-arrow-right-24: Goto Coursier's website](https://get-coursier.io/docs/cli-installation){target="_blank"}

</div>
///

## IDE Setup

Many IDEs support Scala development. The most popular are [VS Code](https://code.visualstudio.com){target="_blank"} and [IntelliJ IDEA](https://www.jetbrains.com/idea){target="_blank"}. We recommend VS Code with the [Metals](https://scalameta.org/metals/){target="_blank"} plugin.

Here is a summary of relevant IDEs:

<div class="grid cards" markdown>

-   :material-microsoft-visual-studio-code:{ .lg .middle } __VS Code (Recommended)__

    ---

    [:octicons-arrow-right-24: Download VS Code](https://code.visualstudio.com/download){target="_blank"}

    [:octicons-arrow-right-24: Scala Development Guide via Metals](https://scalameta.org/metals/docs/editors/vscode#installation){target="_blank"}
    

-   :simple-intellijidea:{ .lg .middle } __IntelliJ IDEA__

    ---

    [:octicons-arrow-right-24: Download IntelliJ via ToolBox](https://www.jetbrains.com/toolbox-app){target="_blank"}

    [:octicons-arrow-right-24: Scala Development Guide](https://www.jetbrains.com/help/idea/get-started-with-scala.html){target="_blank"}

-   :simple-vim:{ .lg .middle } __Vim__

    ---

    [:octicons-arrow-right-24: Download Vim](https://www.vim.org/download.php){target="_blank"}

    [:octicons-arrow-right-24: Scala Development Guide via Metals](https://scalameta.org/metals/docs/editors/vim#nvim-metals){target="_blank"}

-   :simple-sublimetext:{ .lg .middle } __Sublime Text__

    ---

    [:octicons-arrow-right-24: Download Sublime Text](https://www.sublimetext.com/download){target="_blank"}

    [:octicons-arrow-right-24: Scala Development Guide via Metals](https://scalameta.org/metals/docs/editors/sublime#installing-the-plugins){target="_blank"}

-   :simple-gnuemacs:{ .lg .middle } __Emacs__

    ---

    [:octicons-arrow-right-24: Download Emacs](https://www.gnu.org/software/emacs/download.html){target="_blank"}

    [:octicons-arrow-right-24: Scala Development Guide via Metals](https://scalameta.org/metals/docs/editors/emacs#installation){target="_blank"}

-   ![](https://scalameta.org/metals/img/scalameta-logo.png){.lg .middle width=25} __Other IDE/Instructions__

    ---

    [:octicons-arrow-right-24: For other OS or instructions please consult the Metals website.](https://scalameta.org/metals/docs/){target="_blank"}

</div>

