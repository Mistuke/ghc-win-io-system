libraries/win-io-mngr_PACKAGE = win-io-mngr
libraries/win-io-mngr_dist-install_GROUP = libraries
$(if $(filter win-io-mngr,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/win-io-mngr,dist-boot,0)))
$(if $(filter win-io-mngr,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/win-io-mngr,dist-install,1)))
$(if $(filter win-io-mngr,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/win-io-mngr,dist-install,2)))
