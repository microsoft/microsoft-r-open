using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using RInstaller.Attributes;

namespace RInstaller.Models
{
    public class ProductStrings
    {
        public string MarketingPrefix { get; set; }

        public string MarketingPostfix { get; set; }

        public string MarketingVersion { get; set; }

        public string MarketingDescriptionPrimary { get; set; }

        public string MarketingDescriptionSecondary { get; set; }

        public string MarketingUrl { get; set; }

        public string ActionHeadline { get; set; }

        public string ConfirmHeadline { get; set; }

        public string ConfirmLineOne { get; set; }

        public string ConfirmLineTwo { get; set; }

        public string ConfirmLineThree { get; set; }

        public string ConfirmRightButton { get; set; }

        public string FinishHeadline { get; set; }

        public string FinishLineOne { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(ActionHeadline))]
        public string InstallActionHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(ConfirmHeadline))]
        public string InstallConfirmHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(ConfirmLineOne))]
        public string InstallConfirmLineOne { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(ConfirmLineTwo))]
        public string InstallConfirmLineTwo { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(ConfirmLineThree))]
        public string InstallConfirmLineThree { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(ConfirmRightButton))]
        public string InstallConfirmRightButton { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(FinishHeadline))]
        public string InstallFinishHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Install, nameof(FinishLineOne))]
        public string InstallFinishLineOne { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(ActionHeadline))]
        public string ModifyActionHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(ConfirmHeadline))]
        public string ModifyConfirmHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(ConfirmLineOne))]
        public string ModifyConfirmLineOne { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(ConfirmLineTwo))]
        public string ModifyConfirmLineTwo { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(ConfirmLineThree))]
        public string ModifyConfirmLineThree { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(ConfirmRightButton))]
        public string ModifyConfirmRightButton { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(FinishHeadline))]
        public string ModifyFinishHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Modify, nameof(FinishLineOne))]
        public string ModifyFinishLineOne { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(ActionHeadline))]
        public string UninstallActionHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(ConfirmHeadline))]
        public string UninstallConfirmHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(ConfirmLineOne))]
        public string UninstallConfirmLineOne { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(ConfirmLineTwo))]
        public string UninstallConfirmLineTwo { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(ConfirmLineThree))]
        public string UninstallConfirmLineThree { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(ConfirmRightButton))]
        public string UninstallConfirmRightButton { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(FinishHeadline))]
        public string UninstallFinishHeadline { get; set; }

        [ProductStringMapping(LaunchAction.Uninstall, nameof(FinishLineOne))]
        public string UninstallFinishLineOne { get; set; }
    }
}
