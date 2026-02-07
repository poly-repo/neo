from setuptools import setup, find_packages

setup(
    name="o-ghsync",
    version="0.1.0",
    packages=find_packages(),
    install_requires=[
        "requests",
    ],
    scripts=['o-ghsync'],
    author="Maven",
    description="Standalone GitHub Sync Tool",
)